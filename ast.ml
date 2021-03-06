(* variables *)
type var = string
type channel = char
(* (E, id), N, (rho, id) *)
type mode = Enclave of int | Normal 

(* Set of mode variables *)
module ModeSet = Set.Make(struct
  type t = mode
  let compare = Pervasives.compare
end)

module VarSet = Set.Make(struct
  type t = var
  let compare = Pervasives.compare
end)

(* sets of condition variables *)
type cndset = VarSet.t

(* Not really a set. List of boolean variables 
   indicating enclave identifiers i.e., 
   eidset[i] = 1 implies i^th enclave 
   killset[i] = 1 implies i^th enclave is killed 
*)
type eidset = int list
type killset = int list

type varloc = Reg of var | Mem of int 

(* maps with variables and locations as keys *)
module VarLocMap = Map.Make(struct
  type t = varloc
  let compare = Pervasives.compare
end)

type policy =
    Low
   |High
   |Top
   |Erase of policy * var * policy
 

type reftype = Mutable | Immutable


(* Enclave Base types *)
type encbasetype = 
    EBtInt                             (* int *)
  | EBtBool                            (* bool *)
  | EBtCond of mode 			(* cond *)
  | EBtRef of mode * reftype * enclabeltype      (* tau ref *)
  | EBtFunc of mode* enccontext * killset*policy * cndset * enccontext*killset   (* func *)

and
enclabeltype = encbasetype * policy   

(* typechecking environments - maps variables to types *)
and  enccontext = enclabeltype VarLocMap.t

(*Quick Hack: Inefficient use of VarLocMap, we can use just use LocMap or something like that *)
type loctype = mode VarLocMap.t


type encexp =
    EVar of var                       (* x *)
  | ELoc of int 		     (* l^ mode *)
  | ELam of mode * enccontext *killset * policy* cndset * enccontext*killset * policy*encstmt (* First mode|-lambda^mode(gpre,killpre, p,u, gpost, killpost, q, s) *)
  | EConstant of int                  (* n *)
  | EPlus of encexp * encexp          (* e1 + e2 *)
  | EModulo of encexp * encexp        (* e1 % e2 *)
  | ETrue 				(* true *)
  | EFalse 				(* false *)
  | EEq of encexp * encexp            (* e1 = e2 *) 
  | ENeq of encexp * encexp            (* e1 != e2 *) 
  | EDeref of encexp
  | EIsunset of int

and  encstmt = 
   EIf of encexp * encstmt * encstmt 
  |ESkip 
  |EAssign of var * encexp 
  |EDeclassify of var * encexp
  |EUpdate of encexp * encexp
  |ESeq of encstmt * encstmt
  |EESeq of (encstmt list)
  |EWhile of encexp * encstmt
  |EOutput of channel * encexp
  |ECall of encexp
  |ESet of int
  |EEnclave of int * encstmt

type progbody = Encexp of encexp | Encstmt of encstmt 
type program = enccontext * encstmt 

