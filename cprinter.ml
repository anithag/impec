open Format
open Cast
open Ast


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
  | CFunc (mu, pre, post) -> Printf.fprintf oc "void (* %a )() " printvar (variable, isrec)
 
let printCtypes oc cntxt =
  VarLocMap.iter (fun key value -> printSingleCtype oc (key,value, false)) cntxt

let printCprog cprog  =
  (* let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "app.c" in *)
  let oc = open_out "app.c" in
  let _  = printCtypes oc (fst cprog) in
  close_out oc
