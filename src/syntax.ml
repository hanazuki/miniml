open Utils

(* ML interpreter / type reconstruction *)
type id = string

let string_of_id = function
  | "" -> "_"
  | x ->
    let c = x.[0] in
    if 'a' <= c && c <= 'z' then x else "( " ^  x ^ " )"

let print_id = print_string $ string_of_id

module IdMap = Map.Make(struct
  type t = id
  let compare = compare
end)

module IdSet = struct
  include Set.Make(
    struct
      type t = id
      let compare = compare
    end
  )
    
  (** tests whether the two sets are disjoint. *)
  let disjoint xs ys =
    for_all (fun x -> mem x ys) xs
end

type exp =
  | VarExp of id
  | UnitExp
  | ILitExp of int
  | BLitExp of bool
  | TupleExp of exp list
  | ListExp of exp list
  | ConsExp of exp * exp
  | IfExp of exp * exp * exp
  | LetExp of pat * exp * exp
  | LetRecExp of pat * exp * exp
  | FunExp of pat * exp
  | AppExp of exp * exp
  | MatchExp of exp * (pat * exp * exp) list
  | AssertExp of exp
  | TypedExp of exp * tyexp
and pat =
  | AnyPat
  | VarPat of id
  | UnitPat
  | ILitPat of int
  | BLitPat of bool
  | TuplePat of pat list
  | ListPat of pat list
  | ConsPat of pat * pat
  | AltPat of pat * pat
  | NamedPat of id * pat
  | TypedPat of pat * tyexp
and tyexp =
  | AnyTyexp
  | VarTyexp of id
  | UnitTyexp
  | IntTyexp
  | BoolTyexp
  | TupleTyexp of tyexp list
  | ListTyexp of tyexp
  | FunTyexp of tyexp * tyexp
  | NamedTyexp of id * tyexp

(** extracts variables of a patern
    @return list of variables
*)
let rec vars_pat = function
  | AnyPat -> []
  | VarPat id -> [id]
  | UnitPat
  | ILitPat _
  | BLitPat _ -> []
  | ListPat ps -> List.flatten (List.map vars_pat ps)
  | ConsPat (ph, pt) -> (vars_pat ph) @ (vars_pat pt)
  | TuplePat ps -> List.flatten (List.map vars_pat ps)
  | AltPat (pl, _) -> (vars_pat pl)
  | NamedPat (id, p) -> (vars_pat p) @ [id]
  | TypedPat (pat, _) -> vars_pat pat
    
type decl =
  | LetDecl of pat * exp
  | LetRecDecl of pat * exp

type program = 
  | Exp of exp
  | Decls of decl list
