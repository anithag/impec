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
				
