open Ast
open Cast

(* Convert an IMPe type C Type. Ignore policies *)
let rec translatetoCtype enclt = 
	match enclt with
		|EBtInt -> CInt
		|EBtBool -> CBool
  		|EBtCond mu -> (CcondRef mu)
 		|EBtRef (mu,rt,lt) -> CRef (mu, rt, (translatetoCtype (fst lt)))
  		|EBtFunc (mu,pre,_,_, _ ,post,_) -> CFunc (mu, (translatetoCcontext pre), (translatetoCcontext post))  

and translatetoCcontext econtext = 
	 VarLocMap.map (fun value -> translatetoCtype (fst value)) econtext
				
let rec translatetoCexp eexp = match eexp with
  | EVar v -> CVar v
  | EConstant n -> CConst n
  | ELam(mu, pre, kpre, p, u, post, kpost, q, s) -> CLambda ([], (translatetoCstmt s))
  | EPlus (l,r) -> CPlus ((translatetoCexp l), (translatetoCexp r))
  | EModulo (l,r) -> CModulo ((translatetoCexp l), (translatetoCexp r))
  | EEq (l,r) -> CEq ((translatetoCexp l), (translatetoCexp r))
  | ENeq (l,r) -> CNeq ((translatetoCexp l), (translatetoCexp r))
  | ETrue ->  CConst 1
  | EFalse ->  CConst 0
  | ELoc l -> CLoc ("l"^(string_of_int l))
  | EDeref e ->  CDeref (translatetoCexp e)
  | EIsunset l ->  CEq (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1))

and translatetoCstmt estmt = match estmt with
   EIf (e,s1,s2) -> CIf ((translatetoCexp e), (translatetoCstmt s1), (translatetoCstmt s2))
  |ESkip  	-> CSkip 
  |EAssign (x,e) -> CAssign (x, (translatetoCexp e))
  |EDeclassify (x, e) -> CAssign (x,(translatetoCexp e))
  |EUpdate (e1, e2) ->CUpdate ((translatetoCexp e1) ,(translatetoCexp e2))
  |EESeq eli  -> CSeq (List.map translatetoCstmt eli)
  |EWhile (e,s) -> CWhile ((translatetoCexp e), (translatetoCstmt s))
  |EOutput (_, e) -> CSkip
  |ECall f -> CCall (translatetoCexp f)
  |ESet l  -> CUpdate (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1))
