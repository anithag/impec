open Ast

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

and flattenexp eexp = match eexp with
  | ELam(mu, pre, kpre, p, u, post, kpost, q, s) -> ELam(mu, pre, kpre, p, u, post, kpost, q, EESeq (flattenseq s))
  | _ -> eexp

