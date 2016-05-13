open Ast
open Cast
open Helper

exception PrintError of string

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

let rec translatetoCexp eexp gammaenc cfunclist = match eexp with
  | EVar v -> (gammaenc, CVar v, cfunclist)
  | EConstant n -> (gammaenc, CConst n, cfunclist)
  | ELam(mu, pre, kpre, p, u, post, kpost, q, s) -> let (gammaenc', cs, cfunclist') = translatetoCstmt s gammaenc cfunclist in 
						    let argslist  = (translatetoCargs pre) in
						    let retargslist = (translatetoCargs post) in
						    let retstname = next_fvar false in
	        				    let retsttypename    = next_struct_name () in
						    let cstruct = CStruct (retsttypename, retstname,retargslist) in	
						    let cret 	= CRet cstruct in
						    let cs'	= extend_c_sequence cs cret in
						    let cfunc = CLambda (mu, next_fvar true, argslist, cstruct, cs') in
						(gammaenc', cfunc, (cfunclist'@[cfunc]))
  | EPlus (l,r) -> let (gammaencl, cl, cfunclistl) =  (translatetoCexp l gammaenc cfunclist) in
		   let (gammaencr, cr, cfunclistr) =  (translatetoCexp r gammaencl cfunclistl) in
		   (gammaencr, CPlus (cl, cr), cfunclistr) 
  | EModulo (l,r) -> let (gammaencl, cl, cfunclistl) =  (translatetoCexp l gammaenc cfunclist) in
		   let (gammaencr, cr, cfunclistr) =  (translatetoCexp r gammaencl cfunclistl) in
		   (gammaencr, CModulo (cl, cr), cfunclistr) 
  | EEq (l,r) -> let (gammaencl, cl, cfunclistl) =  (translatetoCexp l gammaenc cfunclist) in
		   let (gammaencr, cr, cfunclistr) =  (translatetoCexp r gammaencl cfunclistl) in
		   (gammaencr, CEq (cl, cr), cfunclistr) 
  | ENeq (l,r) -> let (gammaencl, cl, cfunclistl) =  (translatetoCexp l gammaenc cfunclist) in
		   let (gammaencr, cr, cfunclistr) =  (translatetoCexp r gammaencl cfunclistl) in
		   (gammaencr, CNeq (cl, cr), cfunclistr) 
  | ETrue ->  (gammaenc, CConst 1, cfunclist)
  | EFalse ->  (gammaenc, CConst 0, cfunclist)
  | ELoc l -> (gammaenc, CLoc ("l"^(string_of_int l)), cfunclist)
  | EDeref e -> let gammaenc', ce, cfunclist' = translatetoCexp e gammaenc cfunclist in
		(gammaenc, CDeref ce, cfunclist')
  | EIsunset l ->  (gammaenc, CEq (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1)), cfunclist)

and translatetoCstmt estmt gammaenc cfunclist = match estmt with
   EIf (e,s1,s2) -> let (gammaenc', ce, cfunclist') = (translatetoCexp e gammaenc cfunclist) in
		    let (gammaenc1, cs1, cfunclist1) = (translatetoCstmt s1 gammaenc' cfunclist') in
		    let (gammaenc2, cs2, cfunclist2) = (translatetoCstmt s2 gammaenc1 cfunclist1) in
			(gammaenc2, CIf (ce, cs1, cs2), cfunclist2)
  |ESkip  	-> (gammaenc, CSkip, cfunclist) 
  |EDeclassify (x, e)  
  |EAssign (x,e) -> let (gammaenc', ce, cfunclist') = (translatetoCexp e gammaenc cfunclist) in
			 (gammaenc', CAssign (x, ce), cfunclist')
  |EUpdate (e1, e2) ->let (gammaenc1, ce1, cfunclist1) = (translatetoCexp e1 gammaenc cfunclist) in
			let (gammaenc2, ce2, cfunclist2) = (translatetoCexp e2 gammaenc1 cfunclist1) in
			(gammaenc2, CUpdate (ce1,ce2), cfunclist2)
  |EESeq eli  -> let rec loop eli cli gammaenc cfunclist = begin match eli with
		| [] -> (gammaenc,  cli, cfunclist)
		| xs::tail -> let (gammaenc', clm, cfunclist') = translatetoCstmt xs gammaenc cfunclist in
				loop tail (cli@[clm]) gammaenc' cfunclist'
		end in
		let (gammaenc', cli, cfunclist') = loop eli [] gammaenc cfunclist in 
		(gammaenc', CSeq cli, cfunclist')
  |EWhile (e,s) -> let (gammaenc', ce, cfunclist' ) = translatetoCexp e gammaenc cfunclist in
		   let (gammaenc'', cs, cfunclist'' )= translatetoCstmt s gammaenc' cfunclist' in
		  (gammaenc'',	CWhile (ce, cs), cfunclist'')

  |EOutput (_, e) -> (gammaenc, CSkip, cfunclist)
  |ECall f -> let (gammaenc', cfunc, cfunclist') = (translatetoCexp f gammaenc cfunclist) in
	      (* get function name *)
	      let fname = get_fname cfunc in
	      let mu =   get_mode cfunc in
		(* Prepare for return values *)
		(* FIXME: What if cfunc is a variable *)
		let  postcontext = get_funcexp_postcontext cfunc in	
		let retargslist = postcontext in
		let retstname = next_fvar false in
	        let retsttypename    = next_struct_name () in
		let cstruct = CStruct (retsttypename, retstname,retargslist) in	
		let precontext = get_funcexp_precontext cfunc in
		let argslist  = precontext in
		let ccall = (CCall (mu, fname,argslist, cstruct)) in
		(* FIXME: What about copying return type? Handle while printing *)
		(gammaenc', ccall, cfunclist')
  |ESet l  -> 
		(gammaenc, CUpdate (CDeref (CLoc ("l"^(string_of_int l))), (CConst 1)), cfunclist)
  |EEnclave (i,s) -> 
		let (gammaenc', precontext, postcontext) = prepare_pre_post_enclave_context s gammaenc VarLocMap.empty VarLocMap.empty in
		let (gammaenc'', ecstmt, cfunclist') = translatetoCstmt s  gammaenc cfunclist in
		let fname = next_fvar true  in
		let cprecontext = (translatetoCargs precontext) in
		let cpostcontext = (translatetoCargs postcontext) in
	        let mu = (Enclave i) in  
		(* Prepare for return values *)
		(* FIXME: What if func is a variable *)
		let retargslist = cpostcontext in
		let retstname = next_fvar false in
		let retsttypename    = next_struct_name () in
		let cstruct = CStruct (retsttypename, retstname,retargslist) in	
		let cret  = CRet cstruct in
		let ecs	  = extend_c_sequence ecstmt cret in
		let cfunc = CLambda ((Enclave i), fname, cprecontext, cstruct, ecs) in
		let ecall = CCall (mu, fname, cprecontext, cstruct) in 
		(* FIXME: What about copying return type? Handle while printing *)
		(gammaenc'', ecall, (cfunclist'@[cfunc])) 


