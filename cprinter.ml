open Format
open Cast
open Ast
open Helper

exception PrintError of string

(* Given a C AST, print it to file *)
let printvar oc (variable, isrec) =
  let varstr = match variable with
		| Reg x -> x
		| Mem i -> "l"^(string_of_int i)
   in
  if (not isrec) then
	Printf.fprintf oc "%s;\n" varstr
  else
	()

let rec printSingleCtype oc (variable, ctyp, isrec) = match ctyp with
  | CBool -> Printf.fprintf oc " bool %a" printvar (variable, isrec) 
  | CInt  -> Printf.fprintf oc " int %a" printvar  (variable, isrec)
  | CRef (mu, rt, ct) -> Printf.fprintf oc " %a *" printSingleCtype (variable, ct, true); 
						if (not isrec) then
							printvar oc (variable, false)
						else
							()
  | CcondRef mu -> Printf.fprintf oc "bool * %a" printvar (variable, isrec)
					(* FIXME: Pointers to function pointers are broken *)
  | CFunc (pre, post) -> Printf.fprintf oc "void (* %a )() " printvar (variable, isrec)
 
let printCtypes oc cntxt =
  VarLocMap.iter (fun key value -> printSingleCtype oc (key,value, false)) cntxt

let rec printCargs ec argslist = match argslist with
 | [] -> ()
 | (varname, ctype)::tail -> let _ = printSingleCtype ec (varname, ctype, false) in
				printCargs ec tail


let rec printCbody oc cprog = match cprog with
 |CUpdate (ce1, ce2)  -> Printf.fprintf oc " *%a = %a; \n"  printCexp ce1  printCexp ce2  
 |CAssign  (v, cexpr) -> Printf.fprintf oc " %s = %a; \n" v printCexp cexpr  
 |CSeq cstmtlist -> List.iter (fun elm -> (printCbody oc elm)) cstmtlist
 |CWhile (cexpr, cstmnt) -> Printf.fprintf oc  "while ( %a ) { \n %a } \n " printCexp cexpr printCbody cstmnt
 |CIf (cexpr, cstmnt1, cstmnt2) -> Printf.fprintf oc " if ( %a ) { \n %a \}  else { \n %a \n } \n" printCexp cexpr printCbody  cstmnt1 printCbody cstmnt2
 |CSkip -> ()
 |CRet cexpr -> ()				
 |CCall (fname, cexpr) -> Printf.fprintf oc " %s = %s() " (get_return_struct_name cexpr) fname

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
  let _  = printCtypes oc cntxt in
  let _ = printCbody oc cprog in 
  let ec = open_out "Enclave.c" in
  let _ = printEnclave ec cfunclist in 
  close_out oc
