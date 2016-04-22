open Ast

(* Enclave Base types *)
type ctype = 
    CInt				(* int *)
  | CBool				(* bool *)
  | CcondRef of mode 			(* cond *)
  | CRef of mode * reftype * ctype 	(* tau ref *)
  | CFunc of mode* ccontext * ccontext  (* func *)

and  ccontext = ctype VarLocMap.t

