open Ast

(* Enclave Base types *)
type ctype = 
    CInt				(* int *)
  | CBool				(* bool *)
  | CcondRef of mode 			(* cond *)
  | CRef of mode * reftype * ctype 	(* tau ref *)
  | CFunc of ccontext * ccontext  	(* func *)
  | CStructtype of ctype list

and  ccontext = ctype VarLocMap.t


type cexp = 
 |CConst of int
 |CVar of var
 |CLoc of var
 |CPlus of cexp * cexp
 |CModulo of cexp * cexp
 |CEq of cexp * cexp
 |CNeq of cexp * cexp
 |CDeref of cexp
 |CLambda of mode * var* (varloc* ctype) list * cexp * cstmt  (* mu, cstruct funcname (argslist) { cstmt} *)
 |CStruct of var * var * (varloc * ctype) list		(* C structure: sttype stvarname *)
 |CMalloc of mode * int					(* l^mu = malloc(size) *)
 
and cstmt = 
   CAssign of var * cexp
 |CUpdate of cexp * cexp
 |CSeq of cstmt list
 |CWhile of cexp * cstmt
 |CIf of cexp * cstmt * cstmt
 |CSkip
 |CRet of cexp				(* Return statement *)
 |CCall of mode * var * (varloc*ctype) list *  cexp  		(* Function name, args list and return structure*) 


type cprogram = ccontext * cstmt
