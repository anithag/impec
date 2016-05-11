open Format
open Cast
open Ast
open Helper
open Cappfuncprinter 
open Unix

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

let rec printSingleCtype oc (variable, ctyp, isrec, isdecl, iscall, isedl) = match ctyp with
  | CBool 
  | CInt  -> if (not iscall) && ((not isedl) || (isedl && (not isrec)))  then 
		Printf.fprintf oc " int %a" printvar  (variable, isrec, isdecl)
	     else if (not iscall) && isedl && isrec then
		Printf.fprintf oc " [user_check] int %a" printvar (variable, isrec, isdecl) 
	     else
			printvar oc (variable, isrec, isdecl) 
  | CRef (mu, rt, ct) -> if (not iscall)  then 
			  let _ = Printf.fprintf oc " %a *" printSingleCtype (variable, ct, true, isdecl,iscall, isedl) in
				  if (not isrec) then
					printvar oc (variable, false, isdecl)
				  else
					()
	     		else
				printvar oc (variable, isrec, isdecl) 
  | CcondRef mu -> if (not iscall) && ((not isedl) || (isedl && (not isrec)))  then 
			 Printf.fprintf oc "int * %a" printvar (variable, isrec, isdecl)
	     	   else if (not iscall) && isedl then
			 Printf.fprintf oc "[user_check] int * %a" printvar (variable, isrec, isdecl)
	     	   else
			printvar oc (variable, isrec, isdecl) 
					(* FIXME: Pointers to function pointers are broken *)
  | CFunc (pre, post) ->if (not iscall) && ((not isedl) || (isedl && (not isrec)))  then 
				Printf.fprintf oc "void (* %a )() " printvar (variable, isrec, isdecl)
	     	        else if (not iscall) && isedl && isrec then
				(*FIXME: [in] is probably efficient *)
				Printf.fprintf oc "[user_check] void (* %a )() " printvar (variable, isrec, isdecl)
	     		else
				printvar oc (variable, isrec, isdecl) 

let rec printSingleCarraytype oc (variable, ctyp, isrec, printbasetype) = match ctyp with
  | CBool 
  | CInt -> if (printbasetype) then
		Printf.fprintf oc " int "
	    else
			()
  | CRef (mu, rt, ct) -> if (not isrec) && (not printbasetype)  then 
			     Printf.fprintf oc " %a %a %a[]" printSingleCarraytype (variable,ct, true, true)  printSingleCarraytype (variable, ct, true, false) printvar (variable, false, false) 
  			 else if isrec && (not printbasetype) then
				(* prints only [] *)
			     Printf.fprintf oc " %a*" printSingleCarraytype (variable, ct, false, false) 
			 else if isrec && printbasetype then
				printSingleCarraytype oc (variable, ct, true, true)
			 else
				(* should not reach here *)
				()
			
  | CcondRef mu -> Printf.fprintf oc "int %a[]" printvar (variable, true,false)

(* Given a structure, print its members *)
let printCstructmemdecl ec = function
|CStruct (sttypename, stvarname, memlist) -> let rec loop memlist = match memlist with 
					  |[] -> ()
					  | (variable, ctyp)::tail -> let _ = Printf.fprintf ec " %a" printSingleCtype (variable, ctyp, false, true, false, false) in
									      loop tail
					  in loop memlist
| _	-> raise (PrintError "Expected structure expression")
 
(* Given a structure, initialize it. Used in function return *)
let printCstructinit ec = function 
 |CStruct (sttypename, stname, memlist) -> let rec loop memlist = begin match memlist with
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
 |CStruct (sttypename, stname, memlist) -> let rec loop memlist = begin match memlist with
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
 let varcntxt = VarLocMap.filter (fun key value -> match key with
				 | Reg _ -> true
				 | Mem _ -> false
				 ) cntxt in
  VarLocMap.iter (fun key value -> printSingleCtype oc (key,value, false, true, false, false)) varcntxt

 
let rec printCargs ec (argslist, iscall, isedl) = match argslist with
 | [] -> ()
 | (varname, ctype)::tail -> let _ = printSingleCtype ec (varname, ctype, false, false, iscall, isedl) in
			     let _ = begin match tail with
				     | [] -> ()
				     | hd::t' -> Printf.fprintf ec ", "
				    end in	
				printCargs ec (tail, iscall, isedl) 

(* FIXME: malloc size *)
let printAppalloc oc (variable,ctyp) = match ctyp with
  | CcondRef mu -> if not (is_mode_enc mu) then
			Printf.fprintf oc " %a={0};\n " printSingleCarraytype (variable, ctyp, false, false)
		   else
			()
  | CRef (mu, rt, ct) -> if not (is_mode_enc mu) then
				Printf.fprintf oc " %a ={0};\n" printSingleCarraytype (variable, ctyp, false, false)
		        else
				()
  | _ -> ()

let printAppmemory oc cntxt = 
 let loccntxt = VarLocMap.filter (fun key value -> match key with
				 | Reg _ -> false 
				 | Mem _ -> true 
				 ) cntxt in
  VarLocMap.iter (fun key value -> printAppalloc oc (key, value)) loccntxt

(* FIXME: malloc size *)
let printEnclavealloc oc (variable,ctyp) = match ctyp with
  | CcondRef mu -> if (is_mode_enc mu) then
			Printf.fprintf oc " %a={0};\n " printSingleCarraytype (variable, ctyp, false, false)
		   else
			Printf.fprintf oc " %a;\n " printSingleCtype (variable, ctyp, false, false, false, false)
  | CRef (mu, rt, ct) -> if (is_mode_enc mu) then
				Printf.fprintf oc " %a ={0};\n" printSingleCarraytype (variable, ctyp, false, false)
		        else
				 Printf.fprintf oc " %a;\n " printSingleCtype (variable, ctyp, false, false, false, false)
  | _ -> ()

let printEnclavememory ec cntxt = 
 let loccntxt = VarLocMap.filter (fun key value -> match key with
				 | Reg _ -> false 
				 | Mem _ -> true 
				 ) cntxt in
  VarLocMap.iter (fun key value -> printEnclavealloc ec (key, value)) loccntxt

let rec printCbody oc cprog = match cprog with
 |CUpdate (ce1, ce2)  -> Printf.fprintf oc " *%a = %a; \n"  printCexp ce1  printCexp ce2  
 |CAssign  (v, cexpr) -> Printf.fprintf oc " %s = %a; \n" v printCexp cexpr  
 |CSeq cstmtlist -> List.iter (fun elm -> (printCbody oc elm)) cstmtlist
 |CWhile (cexpr, cstmnt) -> Printf.fprintf oc  " while ( %a ) { \n %a } \n " printCexp cexpr printCbody cstmnt
 |CIf (cexpr, cstmnt1, cstmnt2) -> Printf.fprintf oc " if ( %a ) { \n %a }  else { \n %a \n } \n" printCexp cexpr printCbody  cstmnt1 printCbody cstmnt2
 |CSkip -> ()
 |CRet cexpr ->	let emptyst = isstructempty cexpr in
		let stvarname = (get_return_struct_name cexpr) in
		if not emptyst then 
			let _	  = Printf.fprintf oc " \n /* Declare return type */ \n" in
			let sttypename    = (get_return_struct_type cexpr) in
			let _	  = Printf.fprintf oc " %s %s;\n" sttypename stvarname in
			let _	  = Printf.fprintf oc " \n /* Prepare return type */\n" in
			let _     = Printf.fprintf oc " %a " printCstructinit cexpr in
			Printf.fprintf oc " return %s;\n" (get_return_struct_name cexpr) 			
		else
			()
 |CCall (mu, fname, argslist, cexpr) -> let emptyst = isstructempty cexpr in
				        let stvarname = (get_return_struct_name cexpr) in
					let _	    = if not emptyst then 
							    let sttypename    = (get_return_struct_type cexpr) in
							    let _ 	  = Printf.fprintf oc "\n  struct %s{ \n" sttypename in 
							    let _  	  = Printf.fprintf oc "  %a " printCstructmemdecl cexpr in
							    let _	  = Printf.fprintf oc "  };\n\n" in
							    let _	  = Printf.fprintf oc " /* Declare return type */\n" in
							    Printf.fprintf oc " %s %s;\n" sttypename stvarname 
						     else
						    	    Printf.fprintf oc " \n"
						     in
						
				        if (is_mode_enc mu) then
						      let _ = if emptyst then 
				    					Printf.fprintf oc " ret = %s(eid" fname 
								else
				    					Printf.fprintf oc " ret = %s(eid, &%s" fname stvarname 
								in
									
				    			let _ = if not ((List.length argslist) = 0) then
							 	       Printf.fprintf oc ", %a);\n " printCargs (argslist, true, false) 
								else
									Printf.fprintf oc ");\n" 
							in ()
							
					 else
				    			let _     = Printf.fprintf oc " %s = %s( %a );\n " stvarname fname printCargs (argslist, true, false) in
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
 |CDeref cexpr -> Printf.fprintf oc "(*%a)" printCexp cexpr
 |CLambda(mu, fname, argslist, cretstruct, s) -> Printf.fprintf oc " %s %s( %a ) { \n %a \n }" (get_return_struct_type cretstruct) fname printCargs (argslist, false, false) printCbody s
 |CStruct (sttypename, structname,  memlist) -> ()
 

let rec printAppHeaders eh (cfunclist, eidlist) = 
 let _ = Printf.fprintf eh "#ifndef _APP_H_\n#define _APP_H_\n" in
 let rec loop cfunclist = match cfunclist with
  | [] -> ()
  | xs::tail -> let _ = begin match xs with
  		| CLambda(mu, fname, argslist, cretstruct, s) -> let stvarname = (get_return_struct_name cretstruct) in
								 (* Structure declaration not required. Edger8r generates signature from  EDL files *)
								 (* let _	  = Printf.fprintf eh " \n /* Declare return structure type */" in
								 let sttypename    = (get_return_struct_type cretstruct) in
								 let _ 	  = Printf.fprintf eh "\n  struct { \n" in 
								 let _ 	  = Printf.fprintf eh "  %a " printCstructmemdecl cretstruct in
								 let _	  = Printf.fprintf eh  "  }%s;\n\n" sttypename in
								 *)
								 (* Declare function only if it is not in enclave mode. For enclave mode declarations EDL files generate stubs *)
								 (*if not (is_mode_enc mu) then
								  Printf.fprintf eh "  %s %s( %a ); \n\n " (get_return_struct_type cretstruct) fname printCargs (argslist, false)
								 else
								 *)	()
		| _ -> raise (PrintError "Function Expected")  
		end in
		loop tail
 in 
 let _ = loop cfunclist in
 let rec loop2 = function
 |[] -> ()
 |eid::tail -> let _ = Printf.fprintf eh "#define ENCLAVE%s_FILE \"enclave%s.signed.so\"\n" (string_of_int eid) (string_of_int eid) in
	       loop2 tail
 in
 let _ = loop2 eidlist in
 Printf.fprintf eh "#endif /* !_APP__H_ */\n"
 
let initNormalLocations eid cntxt fname = 
		let retstname = next_fvar false in
		let retsttypename    = next_struct_name () in
		let cstruct = CStruct (retsttypename, retstname, []) in	
		let rec loop es loclist = match loclist with
			| [] -> es
			| (l, ctyp)::tail -> let lval, rval = begin match l with
					   			| Mem loc -> "l"^(string_of_int loc),  "_l"^(string_of_int loc)  
								| _	  -> raise (PrintError "Location expected")
							    end in
					   let est = CAssign (lval, CVar rval) in
					   loop (es@[est]) tail
		in
 	       let loccntxt = VarLocMap.filter (fun key value -> match key with
				 | Reg _ -> false 
				 | Mem _ -> true 
				 ) cntxt in
		let eslist  = loop [] (VarLocMap.bindings loccntxt) in
		let cfunc = CLambda ((Enclave eid), fname, (shadowlocations (VarLocMap.bindings loccntxt)), cstruct, CSeq eslist) in
		let ecall = CCall ((Enclave eid), fname, (VarLocMap.bindings loccntxt), cstruct) in 
		(ecall, cfunc)
	
	 

let rec printEnclave ec (cntxt, cfunclist, eid) = 
 let _ = Printf.fprintf ec "#include \"Enclave%s.h\"\n" (string_of_int eid) in 
 let _ = Printf.fprintf ec "#include \"Enclave%s_t.h\"\n\n" (string_of_int eid) in 
 let _ = Printf.fprintf ec "\n/* Use arrays since malloc outside functions gives non-constant initialization error */ \n" in 
 let _  = printEnclavememory ec cntxt in
 let _ = Printf.fprintf ec "\n/* End of allocation */ \n\n" in 
 let rec loop cfunclist = match cfunclist with
  | [] -> ()
  | xs::tail -> let _ = begin match xs with
  		| CLambda(mu, fname, argslist, cretstruct, s) -> let emptyst = isstructempty cretstruct in
								 if emptyst then
									Printf.fprintf ec " void %s( %a ) { \n %a \n }\n" fname printCargs (argslist, false, false) printCbody s
								 else
									Printf.fprintf ec " %s %s( %a ) { \n %a \n }\n" (get_return_struct_type cretstruct) fname printCargs (argslist, false, false) printCbody s
		| _ -> raise (PrintError "Function Expected")  
		end in
		loop tail
 in 
 loop cfunclist
    
let rec printEnclaveHeaders eh (cfunclist, eid) = 
 let _ = Printf.fprintf eh "#ifndef _ENCLAVE%s_H_ \n#define _ENCLAVE%s_H_\n" (string_of_int eid) (string_of_int eid) in
 let rec loop cfunclist = match cfunclist with
  | [] -> ()
  | xs::tail -> let _ = begin match xs with
  		| CLambda(mu, fname, argslist, cretstruct, s) -> if (is_mode_enc mu) then 
									(* Structure declaration not required. Edger8r generates signature from  EDL files *)
									(* let stvarname = (get_return_struct_name cretstruct) in
									 let _	  = Printf.fprintf eh " \n /* Declare return structure type */" in
									 let sttypename    = (get_return_struct_type cretstruct) in
									 let _ 	  = Printf.fprintf eh "\n  struct { \n" in 
									 let _  	  = Printf.fprintf eh "  %a " printCstructmemdecl cretstruct in
									 let _	  = Printf.fprintf eh  "  }%s;\n\n" sttypename in
									  Printf.fprintf eh "  %s %s( %a ); \n\n " (get_return_struct_type cretstruct) fname printCargs (argslist, false)
									 *) ()
								else
									()
		| _ -> raise (PrintError "Function Expected")  
		end in
		loop tail
 in 
 let _ = loop cfunclist in
 Printf.fprintf eh "#endif /* !_ENCLAVE%s_H_ */\n" (string_of_int eid)
 
    
let rec printEDL  edl cfunclist = 
 let _ = Printf.fprintf edl "enclave { \n " in
 let rec loop cfunclist = match cfunclist with
  | [] -> ()
  | xs::tail -> let _ = begin match xs with
  		| CLambda(mu, fname, argslist, cretstruct, s) -> let emptyst = isstructempty cretstruct in
								 let stvarname = (get_return_struct_name cretstruct) in 
								 let _  = if not emptyst then
										 let _	  = Printf.fprintf edl " \n /* Declare return structure type */" in
										 let sttypename    = (get_return_struct_type cretstruct) in
										 let _ 	  = Printf.fprintf edl "\n  struct %s { \n" sttypename in 
										 let _  	  = Printf.fprintf edl "  %a " printCstructmemdecl cretstruct in
										 Printf.fprintf edl  "  };\n\n" 
									   else
										 ()
									   in
								 	   if (is_mode_enc mu) then
										let _ = Printf.fprintf edl " trusted { \n " in
										let _ = if emptyst then
												Printf.fprintf edl " public void %s( %a ); \n\n " fname printCargs (argslist, false, true) 
											 else
												Printf.fprintf edl " public %s %s( %a ); \n\n " (get_return_struct_type cretstruct) fname printCargs (argslist, false, true) 
											 in
										Printf.fprintf edl " }; \n \n " 
									     else
										()
		| _ -> raise (PrintError "Function Expected")  
		end in
		loop tail
  	in 
  let _ =  loop cfunclist in
  Printf.fprintf edl " }; /* End of edl declarations */ \n" 

let rec printAppUheaders oc eidlist = match eidlist with
 |[] -> ()
 | eid::tail -> 
  		let _ = Printf.fprintf oc "#include \"Enclave%s_u.h\"\n" (string_of_int eid) in 
		printAppUheaders oc tail

let printCprog (cntxt, cprog, cfunclist) =
  (* let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "app.c" in *)
  let eidlist = getusedEnclaves [] cfunclist  in
  let output = "encproject" in
  let _ =  (try Unix.rmdir output with Unix_error(err, _,_)->())  in
  let _ = (try Unix.mkdir output 0o777 with Unix_error(err, _,_)->())  in
  let oh = open_out (output^"/app.h") in
  let _ = printAppHeaders oh (cfunclist, eidlist) in 
  let oc = open_out (output^"/app.c") in
  let _ = Printf.fprintf oc "#include <stdio.h>\n" in 
  let _ = Printf.fprintf oc "#include \"app.h\"\n" in 
  let _ = printAppUheaders oc eidlist in 
  let _ = Printf.fprintf oc "#include \"sgx_urts.h\"\n\n" in 
  (* Global memory locations *)
  let _  = printAppmemory oc cntxt in
  let _  = Printf.fprintf oc " /* Application Entry */\n int SGX_CDECL main(int argc, char *argv[]) {\n" in
  (* Variable declaration *)
  let _  = printCtypes oc cntxt in 
  let _  = Printf.fprintf oc "%s\n" enclave_status_var_str in
  let _  = Printf.fprintf oc " /* End of variable declaration. Begin enclave load.  */ \n\n "  in
  (* let _  = Printf.fprintf oc "%s\n" load_enclave_str in *)
  let rec loop1 eidlist = begin match eidlist with
   |[] -> ()
   |eid::tail -> let _ = load_enclave_str oc  eid in
		 loop1 tail
   end in
  let _ = loop1 eidlist in
  let _  = Printf.fprintf oc " /* End of enclave load. \n "  in
  let _  = Printf.fprintf oc "   Begin ECALLs from application.\n */"  in
  let rec loop2 cprog cfunclist eidlist = match eidlist with
	| [] -> (cprog, cfunclist)
   	| eid::tail -> 
		let fname = "initmemloc"^(string_of_int eid) in
  		let (ecall, cfunc) = initNormalLocations  eid cntxt fname in
		let cprog' = (prepend_c_sequence  cprog ecall) in 
		loop2 cprog' (cfunclist@[cfunc]) tail
	in 
  let (cprog', cfunclist'') = loop2 cprog [] eidlist in 
  let cfunclist' = (cfunclist''@cfunclist) in
  let _  = printCbody oc cprog' in
  let rec loop3 eidlist = begin match eidlist with
   |[] -> ()
   |eid::tail -> let _ = destroy_enclave_str oc  eid in
		 loop3 tail
   end in
  let _ = loop3 eidlist in
  let _  = Printf.fprintf oc " \n } /*End of main() */ \n" in
  let rec createEnclavefiles eidlist= begin match eidlist with
	|[] -> ()
 	|eid::tail -> 
	  let eh = open_out (output^"/Enclave"^(string_of_int eid)^".h") in
	  let _ = printEnclaveHeaders eh (cfunclist', eid) in 
	  let ec = open_out (output^"/Enclave"^(string_of_int eid)^".c") in
	  let _ = printEnclave ec (cntxt, cfunclist', eid) in 
	  let edl = open_out (output^"/Enclave"^(string_of_int eid)^".edl") in
	  let _ = printEDL edl cfunclist' in
	  close_out ec;
	  close_out eh;
	  close_out edl;
	  createEnclavefiles tail
  end in
  let _ = createEnclavefiles eidlist in
  close_out oc;
  close_out oh
