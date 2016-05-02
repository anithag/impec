open Format
open Cast
open Ast
open Helper
open Cappfuncprinter 

exception PrintError of string

(* Given a C AST, print it to file *)
let printvar oc (variable, isrec, isdecl) =
  let varstr = match variable with
		| Reg x -> x
		| Mem i -> "l"^(string_of_int i)
   in
  if (not isrec) && isdecl then
	Printf.fprintf oc " %s;\n" varstr
  else if (not isrec) && (not isdecl) then
	(* Print function arguments *)
	Printf.fprintf oc "%s" varstr
  else
	()

let rec printSingleCtype oc (variable, ctyp, isrec, isdecl) = match ctyp with
  | CBool -> Printf.fprintf oc " bool %a" printvar (variable, isrec, isdecl) 
  | CInt  -> Printf.fprintf oc " int %a" printvar  (variable, isrec, isdecl)
  | CRef (mu, rt, ct) -> Printf.fprintf oc " %a *" printSingleCtype (variable, ct, true, isdecl); 
						if (not isrec) then
							printvar oc (variable, false, isdecl)
						else
							()
  | CcondRef mu -> Printf.fprintf oc "bool * %a" printvar (variable, isrec, isdecl)
					(* FIXME: Pointers to function pointers are broken *)
  | CFunc (pre, post) -> Printf.fprintf oc "void (* %a )() " printvar (variable, isrec, isdecl)


let printCstructdecl ec = function
|CStruct (stvarname, memlist) -> let rec loop memlist = match memlist with 
				  |[] -> ()
				  | (variable, ctyp)::tail -> let _ = Printf.fprintf ec " %a" printSingleCtype (variable, ctyp, false, true) in
							      loop tail
				in loop memlist

				 
| _	-> raise (PrintError "Expected structure expression")
 
(* Given a structure, initialize it. Used in function return *)
let printCstructinit ec = function 
 |CStruct (stname, memlist) -> let rec loop memlist = begin match memlist with
				|[] -> ()
				|(variable, _)::tail -> let _ = begin match variable with
								| Reg x -> Printf.fprintf ec " %s.%s = %s;\n" stname x x 
								| Mem i -> let locname = "l"^(string_of_int i)  in
									Printf.fprintf ec " %s.%s = %s;\n" stname locname locname 
								end in
								loop tail
				end in loop memlist
 | _ -> raise (PrintError "Expected structure expression")

(* Given a structure, initialize variables to its member values. Used after function call *)
let printCstructinitRev ec = function 
 |CStruct (stname, memlist) -> let rec loop memlist = begin match memlist with
				|[] -> ()
				|(variable, _)::tail -> let _ = begin match variable with
								| Reg x -> Printf.fprintf ec " %s = %s.%s;\n" x stname x 
								| Mem i -> let locname = "l"^(string_of_int i)  in
									Printf.fprintf ec " %s = %s.%s;\n" locname stname locname 
								end in
								loop tail
				end in loop memlist
 | _ -> raise (PrintError "Expected structure expression")


let printCtypes oc cntxt =
  VarLocMap.iter (fun key value -> printSingleCtype oc (key,value, false, true)) cntxt

 
let rec printCargs ec argslist = match argslist with
 | [] -> ()
 | (varname, ctype)::tail -> let _ = printSingleCtype ec (varname, ctype, false, false) in
			     let _ = begin match tail with
				     | [] -> ()
				     | hd::t' -> Printf.fprintf ec ", "
				    end in	
				printCargs ec tail

let rec printCbody oc cprog = match cprog with
 |CUpdate (ce1, ce2)  -> Printf.fprintf oc " *%a = %a; \n"  printCexp ce1  printCexp ce2  
 |CAssign  (v, cexpr) -> Printf.fprintf oc " %s = %a; \n" v printCexp cexpr  
 |CSeq cstmtlist -> List.iter (fun elm -> (printCbody oc elm)) cstmtlist
 |CWhile (cexpr, cstmnt) -> Printf.fprintf oc  " while ( %a ) { \n %a } \n " printCexp cexpr printCbody cstmnt
 |CIf (cexpr, cstmnt1, cstmnt2) -> Printf.fprintf oc " if ( %a ) { \n %a }  else { \n %a \n } \n" printCexp cexpr printCbody  cstmnt1 printCbody cstmnt2
 |CSkip -> ()
 |CRet cexpr ->	let stvarname = (get_return_struct_name cexpr) in
		let _	  = Printf.fprintf oc " \n /* Declare return type */" in
	        let sttypename    = next_struct_name () in
	    	let _ 	  = Printf.fprintf oc "\n  typedef struct { \n" in 
		let _  	  = Printf.fprintf oc "  %a " printCstructdecl cexpr in
		let _	  = Printf.fprintf oc "  }%s;\n" sttypename in
		let _	  = Printf.fprintf oc " %s %s;\n" sttypename stvarname in
		let _	  = Printf.fprintf oc " \n /* Prepare return type */\n" in
		let _     = Printf.fprintf oc " %a " printCstructinit cexpr in
		Printf.fprintf oc " return %s;\n" (get_return_struct_name cexpr) 			
 |CCall (fname, argslist, cexpr) -> let stvarname = (get_return_struct_name cexpr) in
				    let sttypename    = next_struct_name () in
				    let _ 	  = Printf.fprintf oc "\n  typedef struct { \n" in 
				    let _  	  = Printf.fprintf oc "  %a " printCstructdecl cexpr in
				    let _	  = Printf.fprintf oc "  }%s;\n\n" sttypename in
				    let _	  = Printf.fprintf oc " /* Declare return type */\n" in
				    let _	  = Printf.fprintf oc " %s %s;\n" sttypename stvarname in
				    let _	  = Printf.fprintf oc " ret = %s(&%s, %a);\n " fname stvarname printCargs argslist in
				    let _	  = Printf.fprintf oc " \n /* Copy return type */\n" in
					Printf.fprintf oc " %a " printCstructinitRev cexpr
				    

and printCexp oc cexpr = match cexpr with
 |CConst i ->Printf.fprintf oc "%s" (string_of_int i)
 |CVar v ->Printf.fprintf oc "%s" v
 |CLoc v ->Printf.fprintf oc "%s" v
 |CPlus (ce1, ce2) ->Printf.fprintf oc "%a + %a" printCexp ce1 printCexp ce2
 |CModulo (ce1,ce2) ->Printf.fprintf oc "%a %% %a" printCexp ce1 printCexp ce2
 |CEq (ce1, ce2) ->Printf.fprintf oc "%a == %a" printCexp ce1 printCexp ce2
 |CNeq (ce1, ce2) ->Printf.fprintf oc "%a != %a" printCexp ce1 printCexp ce2
 |CDeref cexpr -> Printf.fprintf oc "*%a" printCexp cexpr
 |CLambda(mu, fname, argslist, retargslist, s) -> Printf.fprintf oc " void %s( %a ) { \n %a \n }" fname printCargs argslist printCbody s
 |CStruct (structname,  memlist) -> ()
 


let rec printEnclave ec cfunclist = match cfunclist with
  | [] -> ()
  | xs::tail -> begin match xs with
  		| CLambda(mu, fname, argslist, retargslist, s) -> Printf.fprintf ec " void %s( %a ) { \n %a \n }" fname printCargs argslist printCbody s
		| _ -> raise (PrintError "Function Expected")  
		end
    
let printCprog (cntxt, cprog, cfunclist) =
  (* let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "app.c" in *)
  let oc = open_out "app.c" in
  let _  = Printf.fprintf oc " /* Application Entry */\n int SGX_CDECL main(int argc, char *argv[]) {\n" in
  let _  = printCtypes oc cntxt in
  let _  = Printf.fprintf oc "%s\n" enclave_status_var_str in
  let _  = Printf.fprintf oc " /* End of variable declaration. Begin enclave load. */ \n\n "  in
  let _  = Printf.fprintf oc "%s\n" load_enclave_str in
  let _  = Printf.fprintf oc " /* End of enclave init. Begin ECALLs from application. */ \n\n "  in
  let _  = printCbody oc cprog in 
  let _  = Printf.fprintf oc " \n } /*End of main() */ \n" in
  let ec = open_out "Enclave.c" in
  let _ = printEnclave ec cfunclist in 
  close_out oc
