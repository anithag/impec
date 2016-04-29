open Ast
open Cast
open Helper

(* Convert an IMPe type C Type. Ignore policies *)
let rec translatetoCtype enclt = 
	match enclt with
		|EBtInt -> CInt
		|EBtBool -> CBool
  		|EBtCond mu -> (CcondRef mu)
 		|EBtRef (mu,rt,lt) -> CRef (mu, rt, (translatetoCtype (fst lt)))
  		|EBtFunc (mu,pre,_,_, _ ,post,_) -> CFunc ((translatetoCcontext pre), (translatetoCcontext post))  

and translatetoCcontext econtext = 
	 VarLocMap.map (fun value -> translatetoCtype (fst value)) econtext
				
let translatetoCargs cntxt = 
 let argsmap = VarLocMap.map (fun value -> translatetoCtype (fst value)) cntxt  in
 VarLocMap.bindings argsmap

let rec translatetoCexp eexp gammaenc = match eexp with
  | EVar v -> (gammaenc, CVar v)
  | EConstant n -> (gammaenc, CConst n)
  | ELam(mu, pre, kpre, p, u, post, kpost, q, s) -> let (gammaenc', cs) = translatetoCstmt s gammaenc in 
							(* FIXME: What about return type? *)
						(gammaenc', CLambda (mu, next_fvar true, (translatetoCargs pre), (translatetoCargs post), cs))
  | EPlus (l,r) -> let (gammaencl, cl) =  (translatetoCexp l gammaenc) in
		   let (gammaencr, cr) =  (translatetoCexp r gammaencl) in
		   (gammaencr, CPlus (cl, cr)) 
  | EModulo (l,r) -> let (gammaencl, cl) =  (translatetoCexp l gammaenc) in
		   let (gammaencr, cr) =  (translatetoCexp r gammaencl) in
		   (gammaencr, CModulo (cl, cr)) 
  | EEq (l,r) -> let (gammaencl, cl) =  (translatetoCexp l gammaenc) in
		   let (gammaencr, cr) =  (translatetoCexp r gammaencl) in
		   (gammaencr, CEq (cl, cr)) 
  | ENeq (l,r) -> let (gammaencl, cl) =  (translatetoCexp l gammaenc) in
		   let (gammaencr, cr) =  (translatetoCexp r gammaencl) in
		   (gammaencr, CNeq (cl, cr)) 
  | ETrue ->  (gammaenc, CConst 1)
  | EFalse ->  (gammaenc, CConst 0)
  | ELoc l -> (gammaenc, CLoc ("l"^(string_of_int l)))
  | EDeref e -> let gammaenc', ce = translatetoCexp e gammaenc in
		(gammaenc, CDeref ce)
  | EIsunset l ->  (gammaenc, CEq (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1)))

and translatetoCstmt estmt gammaenc = match estmt with
   EIf (e,s1,s2) -> let (gammaenc', ce) = (translatetoCexp e gammaenc) in
		    let (gammaenc1, cs1) = (translatetoCstmt s1 gammaenc') in
		    let (gammaenc2, cs2) = (translatetoCstmt s2 gammaenc1) in
			(gammaenc2, CIf (ce, cs1, cs2))
  |ESkip  	-> (gammaenc, CSkip) 
  |EDeclassify (x, e)  
  |EAssign (x,e) -> let (gammaenc', ce) = (translatetoCexp e gammaenc) in
			 (gammaenc', CAssign (x, ce))
  |EUpdate (e1, e2) ->let (gammaenc1, ce1) = (translatetoCexp e1 gammaenc) in
			let (gammaenc2, ce2) = (translatetoCexp e2 gammaenc1) in
			(gammaenc2, CUpdate (ce1,ce2))
  |EESeq eli  -> let rec loop eli cli gammaenc = begin match eli with
		| [] -> (gammaenc,  cli)
		| xs::tail -> let (gammaenc', clm) = translatetoCstmt xs gammaenc in
				loop tail (cli@[clm]) gammaenc'
		end in
		let (gammaenc', cli) = loop eli [] gammaenc in 
		(gammaenc', CSeq cli)
  |EWhile (e,s) -> let (gammaenc', ce ) = translatetoCexp e gammaenc in
		   let (gammaenc'', cs )= translatetoCstmt s gammaenc' in
		  (gammaenc'',	CWhile (ce, cs))

  |EOutput (_, e) -> (gammaenc, CSkip)
  |ECall f -> let (gammaenc', cfunc) = (translatetoCexp f gammaenc) in
	      (* get function name *)
	      let fname = get_fname cfunc in
		(* Prepare for return values *)
		(* FIXME: What if cfunc is a variable *)
		let  postcontext = get_funcexp_postcontext cfunc in	
		let retargslist = postcontext in
		let retstname = next_fvar false in
		let cstruct = CStruct (retstname,retargslist) in	
		let ccall = (CCall (fname,cstruct)) in
		(* FIXME: What about copying return type? *)
		(gammaenc', ccall)
  |ESet l  -> 
		(gammaenc, CUpdate (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1)))
  |EEnclave (i,s) -> 
		let (gammaenc', precontext, postcontext) = prepare_pre_post_enclave_context s gammaenc VarLocMap.empty VarLocMap.empty in
		let (gammaenc'', ecstmt) = translatetoCstmt s  gammaenc in
		let fname = next_fvar true  in
		let cfunc = CLambda ((Enclave i), fname, (translatetoCargs precontext), (translatetoCargs postcontext), ecstmt) in
		(* Prepare for return values *)
		(* FIXME: What if func is a variable *)
		let  postcontext = get_funcexp_postcontext cfunc in	
		let retargslist = postcontext in
		let retstname = next_fvar false in
		let cstruct = CStruct (retstname,retargslist) in	
		let ecall = CCall (fname, cstruct) in 
		(* FIXME: What about copying return type? *)
		(gammaenc'', ecall) 
