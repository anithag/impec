open Ast
open Cast

exception HelperError of string
exception TypeNotFoundError
exception TypeError

let rec flattenseq s = match s with
  |ESeq(s1, s2) -> flattenseq s1 @ flattenseq s2
  |EIf (e,s1,s2) -> [EIf ((flattenexp e), EESeq (flattenseq s1), EESeq (flattenseq s2))]
  |ESkip  	->  [ESkip]
  |EAssign (x,e) -> [EAssign (x, e)]
  |EDeclassify (x, e) -> [EDeclassify (x, (flattenexp e))]
  |EUpdate (e1, e2) -> [EUpdate ((flattenexp e1), (flattenexp e2))]
  |EESeq eli  -> eli
  |EWhile (e,s) ->  [EWhile ((flattenexp e), EESeq (flattenseq s))]
  |EOutput (c, e) -> [EOutput(c, (flattenexp e))]
  |ECall f -> [ECall (flattenexp f)]
  |ESet x  -> [ESet x]
  |EEnclave (i,s) -> [EEnclave (i, EESeq (flattenseq s))]

and flattenexp eexp = match eexp with
  | ELam(mu, pre, kpre, p, u, post, kpost, q, s) -> ELam(mu, pre, kpre, p, u, post, kpost, q, EESeq (flattenseq s))
  | _ -> eexp

let get_return_struct_name = function
  | CStruct (stname, memlist) -> stname
  | _ -> raise (HelperError "Expected structure")

let get_fname = function
 |CLambda (mu, f, _ ,_,_) -> f
 | _  -> raise (HelperError "Lambda expression expected")

let get_enc_precontext (t:enclabeltype) =
   match t with
   |EBtFunc(m,gencpre,_,p,u, gencpost,_), q -> gencpre
   |_ -> raise (HelperError "")

let get_enc_postcontext (t:enclabeltype) =
   match t with
   |EBtFunc(m,gencpre,_,p,u, gencpost,_), q -> gencpost
   |_ -> raise (HelperError "")

let get_funcexp_postcontext = function
 |CLambda (mu, f, pre , post,_) -> post
 | _ -> raise (HelperError "Function Expression expected") 

let rec get_enc_exp_type (genc:enccontext) (e:encexp) : enclabeltype =
  match e with
   | EVar x  -> (try VarLocMap.find (Reg x) genc with Not_found -> raise TypeNotFoundError)
   | ELoc l -> (try VarLocMap.find (Mem l) genc with Not_found -> raise TypeNotFoundError)
   | ELam(mu, gpre, kpre, p,u, gpost, kpost, q,s) -> (EBtFunc(mu,gpre, kpre, p,u, gpost, kpost), q) 
   | EConstant n -> (EBtInt, Low)
   | ETrue     -> (EBtBool, Low)
   | EFalse  -> (EBtBool, Low)
   | EEq(e1, e2) 
   | ENeq(e1, e2) -> 
			(* FIXME: Quick Hack...policies do not matter, they get erased. Make them low as quick hack. Remove this later *)
			(EBtBool, Low)
   | EPlus(e1, e2) 
   | EModulo(e1, e2) -> (EBtInt, Low)
   | EIsunset x -> (EBtBool, Low)
   | EDeref  e   -> begin match (get_enc_exp_type genc e) with
		  | (EBtRef(_, rt, lt), p) -> (fst lt, Low)
		  | _  -> raise TypeError
		 end
 
let rec get_enc_exp_label lt = (snd lt)

let rec enc_flow_sensitive_type_infer (pc:policy) (genc:enccontext) = function
    |EAssign(x, e)-> 
		      let enctype = get_enc_exp_type  genc e in
		      let encvarlabtype = Low in
		      let genc1 = VarLocMap.add (Reg x) (fst enctype, encvarlabtype) genc in
		      genc1
    |EDeclassify(x, e) -> 
		      let enctype = get_enc_exp_type  genc e in
		      let genc1 = VarLocMap.add (Reg x) (fst enctype, Low) genc in
		      genc1
    |EUpdate( e1, e2) -> genc
    |ESeq( s1, s2)  ->  let g1 = enc_flow_sensitive_type_infer pc genc s1 in
		            let g2 = enc_flow_sensitive_type_infer pc g1 s2 in
		      	    g2 
    |EESeq(slist)  -> let rec seq_flow_sensitive genc = function
				|[] -> genc
				|s::tail ->
			    		let genc' = enc_flow_sensitive_type_infer pc genc s in
					seq_flow_sensitive genc' tail
			   in seq_flow_sensitive genc slist

    |ESet(x)	-> genc
    |ESkip -> genc
    |EIf(e, s1, s2) ->   
		      let pc' = get_enc_exp_label (get_enc_exp_type genc e) in
    		      let g1 = enc_flow_sensitive_type_infer pc' genc s1 in
		      let g2 = enc_flow_sensitive_type_infer pc' genc s2 in
		      let g' = VarLocMap.merge (fun key bp1 bp2 -> 
			begin match (bp1, bp2)  with
			|(Some (b1, p1), Some (b2, p2)) -> 
				Some (b1, Low)
			| _ -> raise TypeNotFoundError
			end ) g1 g2
			in g'
    |EWhile(e, s) -> (* Compute Fixpoint *)
		    let rec compute_fixpoint s gi'  = 
		    	 let pc' = Low in
			 let gi'' = enc_flow_sensitive_type_infer pc' gi' s in
		         let gn = VarLocMap.merge (fun key bp1 bp2 -> 
						begin match (bp1, bp2)  with
						|(Some (b1, p1), Some (b2, p2)) -> 
								Some (b1, Low)
						| _ -> raise TypeNotFoundError
						end ) genc gi''
			in 
			 if (VarLocMap.equal (fun a b -> if a = b then true else false) gi' gi'') then
			 	gn
			 else 
				compute_fixpoint s  gn
		     in compute_fixpoint s genc
    |ECall (e) ->  let enctype = get_enc_exp_type genc e in
			let gencpost = get_enc_postcontext enctype in
			(* Genc_out(x) = Genc_post(x) if x is in Dom(Genc_post)
			    = Genc(x) o.w *)
			let gencout = VarLocMap.merge (fun key left right -> begin match (left, right) with
							| Some x, Some y -> left
							| None, right -> right 
							| left, None  -> None (* error *)
							end) gencpost genc
			in
			gencout
    |EOutput(x, e) -> genc
    |EEnclave(i, s) -> enc_flow_sensitive_type_infer pc genc s   


let get_mode_ref = function
 | EBtRef (mu,_,_), _ -> mu
 | _,_ -> raise (HelperError "Expected Ref")

let is_mode_enc = function
 | Enclave _ -> true
 | _ -> false

let rec prepare_pre_post_enclave_context_exp e encgamma pre post isdef = 
 match e with
  | EVar v -> if isdef then
		(pre, VarLocMap.add (Reg v) (VarLocMap.find (Reg v) encgamma) post)
	      else
		(VarLocMap.add (Reg v) (VarLocMap.find (Reg v) encgamma) pre, post)
  | EIsunset l 
  | ELoc l -> (* Optimization: Add only if it is non-enclave location *)
		let  loctype = VarLocMap.find (Mem l) encgamma in
		let mu = get_mode_ref loctype in
		if (is_mode_enc mu) then
			(* enclave locations do not flow outside *)
			(pre, post)	
		else if isdef then
			(pre, VarLocMap.add (Mem l) loctype post)
		else
			(VarLocMap.add (Mem l) loctype pre, post)
  | EConstant n -> (pre, post)
  | ELam(mu, fpre, kpre, p, u, fpost, kpost, q, s) -> (pre, post)
  | EPlus (l,r) 
  | EModulo (l,r)
  | EEq (l,r) 
  | ENeq (l,r) -> let (pre1, post1) = prepare_pre_post_enclave_context_exp l encgamma pre post isdef in
		prepare_pre_post_enclave_context_exp r encgamma pre1 post1 isdef
  | ETrue 
  | EFalse ->  (pre, post)
  | EDeref e ->  prepare_pre_post_enclave_context_exp e encgamma pre post isdef

let rec prepare_pre_post_enclave_context s encgamma pre post = 
 match s with
   EIf (e,s1,s2) -> let (pre', post') =  prepare_pre_post_enclave_context_exp e encgamma pre post false in
		    let (encgamma1, pre1, post1) = prepare_pre_post_enclave_context s1 encgamma pre' post' in
			prepare_pre_post_enclave_context s2 encgamma1 pre1 post1 
  |ESkip  	-> (encgamma, pre, post)
  |EDeclassify (x, e)  
  |EAssign (x,e) -> 
		let (pre', post') =  prepare_pre_post_enclave_context_exp e encgamma pre post false in
		let encgamma' = enc_flow_sensitive_type_infer  Low  encgamma s in
		let xtype = VarLocMap.find (Reg x) encgamma' in
		(encgamma', pre', VarLocMap.add (Reg x) xtype post')
  |EUpdate (e1, e2) ->
		let (pre1, post1) =  prepare_pre_post_enclave_context_exp e1 encgamma pre post true in
		let (pre2, post2) =  prepare_pre_post_enclave_context_exp e2 encgamma pre1 post1 false in
		(encgamma, pre2, post2)
  |EESeq eli  -> let rec loop eli encgamma pre post = begin match eli with
		 |[] -> (encgamma, pre, post)
		 |xs::tail -> let (encgamma', pre', post') = prepare_pre_post_enclave_context xs encgamma pre post in
				loop tail encgamma' pre' post'
		end
		in
		loop eli encgamma  pre post

  |EWhile (e,s) -> 
		let (pre', post') =  prepare_pre_post_enclave_context_exp e encgamma pre post false in
		prepare_pre_post_enclave_context s encgamma pre' post'

  |EOutput (_, e) -> 
		let (pre', post') =  prepare_pre_post_enclave_context_exp e encgamma pre post false in
		(encgamma, pre', post')
  |ECall f -> 
		let (pre', post') =  prepare_pre_post_enclave_context_exp f encgamma pre post false in
		let encgamma' = enc_flow_sensitive_type_infer Low encgamma s in
		(encgamma', pre', post')
  |ESet l  -> 
		let (pre1, post1) =  prepare_pre_post_enclave_context_exp (ELoc l) encgamma pre post true in
		(encgamma, pre1, post1)
  |EEnclave (i,s) -> raise (HelperError "Nested Enclaves found")

(* ------ Function Names ---------- *)
let fvar_cell = ref 1

(* [next_fvar isfunc ] generates a fresh type variable *)
let next_fvar isfunc : var =
  let x = !fvar_cell in
  let funcvar = if isfunc then
		"f" 
	  	else  "stvar" in
  let s = funcvar ^ string_of_int x in
  incr fvar_cell;  s

