open Syntax
open Utils

type exval =
  | UnitV
  | IntV of int
  | BoolV of bool
  | FunV of pat * exp * dnval Env.t
  | TupleV of exval list
  | ListV of exval list
  | MonadV of (dnval lazy_t -> dnval)
and dnval = exval

type env = dnval Env.t

exception Error of string
exception Match_fail

let err s = raise (Error s)

let string_of_exval exval =
  let buffer = Buffer.create 100 in
  let rec str = function
    | UnitV ->
      Buffer.add_string buffer "()"
    | IntV i ->
      Buffer.add_string buffer (string_of_int i)
    | BoolV b ->
      Buffer.add_string buffer (if b then "true" else "false")
    | FunV _ | MonadV _ ->
      Buffer.add_string buffer "<fun>"
    | TupleV l ->
      Buffer.add_char buffer '(';
      strlist ", " l;
      Buffer.add_char buffer ')'
    | ListV l ->
      Buffer.add_char buffer '[';
      strlist "; " l;
      Buffer.add_char buffer ']'
  and strlist sep = function
      | [] -> ()
      | [v] -> str v
      | v :: vs -> str v; Buffer.add_string buffer sep; strlist sep vs
  in
  str exval;
  Buffer.contents buffer

let print_exval = print_string $ string_of_exval

(** Matches pattern against a value
    @param v value
    @param p pattern
    @return bindings
*)
let rec match_pat v p =
  match p, v with
    | AnyPat, _ -> []
    | VarPat id, v -> [id, v]
    | UnitPat, UnitV -> []
    | UnitPat, _ -> assert false
    | ILitPat i, IntV j when i = j -> []
    | ILitPat _, _ -> raise Match_fail
    | BLitPat b, BoolV c when b = c -> []
    | BLitPat _, _ -> raise Match_fail
    | TuplePat ps, TupleV vs when List.length ps = List.length vs ->
      List.fold_left2 (fun binds v p -> binds @ match_pat v p) [] vs ps
    | TuplePat _, _ -> raise Match_fail
    | ListPat ps, ListV vs when List.length ps = List.length vs ->
      List.fold_left2 (fun binds v p -> binds @ match_pat v p) [] vs ps
    | ListPat _, _ -> raise Match_fail
    | ConsPat (p, ps), ListV (v :: vs) ->
      match_pat v p @ match_pat (ListV vs) ps
    | ConsPat _, _ -> raise Match_fail
    | NamedPat (id, p), v -> match_pat v p @ [id, v]
    | AltPat (p1, p2), v ->
      (try match_pat v p1 with
        | Match_fail -> match_pat v p2)
    | TypedPat (pat, _), v ->
      match_pat v pat

let rec eval_exp env = function
  | VarExp x ->
    (try Env.lookup x env with
      | Env.Not_bound -> assert false)
  | UnitExp -> UnitV
  | ILitExp i -> IntV i
  | BLitExp b -> BoolV b
  | TupleExp es -> TupleV (List.map (eval_exp env) es)
  | ListExp l -> ListV (List.map (eval_exp env) l)
  | ConsExp (hd, tl) ->
    (match eval_exp env hd, eval_exp env tl with
      | x, ListV l -> ListV (x :: l)
      | _ -> assert false)
  | IfExp (exp1, exp2, exp3) ->
    (match eval_exp env exp1 with
      | BoolV true -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> assert false)
  | LetExp (pat, expb, expe) ->
    let vb = eval_exp env expb in
    let binds = match_pat vb pat in
    let env = List.fold_left (fun env (id, v) -> Env.extend id v env) env binds in
    eval_exp env expe
  | LetRecExp (pat, expb, expe) ->
    let vars = vars_pat pat in
    let env = List.fold_left (fun env id -> Env.reserve id env) env vars in
    let vb = eval_exp env expb in
    let binds = match_pat vb pat in
    List.iter (fun (id, v) -> Env.emplace id v env) binds;
    eval_exp env expe
  | FunExp (pat, body) -> FunV (pat, body, env)
  | AppExp (expf, expa) ->
    (match eval_exp env expf with
      | FunV (pat, expb, frame) ->
        let binds = match_pat (eval_exp env expa) pat in
        let env = List.fold_left (fun env (id, v) -> Env.extend id v env) frame binds in
        eval_exp env expb
      | MonadV monad -> monad (lazy (eval_exp env expa))
      | _ -> assert false)
  | MatchExp (exp, matchings) ->
    let v = eval_exp env exp in
    let rec do_match = function
      | [] -> err "Matching failed"
      | (pat, exp, grd) :: rest ->
        try
          let binds = match_pat v pat in
          let env = List.fold_left (fun env (id, v) -> Env.extend id v env) env binds in
          match eval_exp env grd with
            | BoolV true -> eval_exp env exp
            | BoolV false -> raise Match_fail
            | _ -> assert false
        with
          | Match_fail -> do_match rest
    in
    do_match matchings
  | AssertExp exp ->
    (match eval_exp env exp with
      | BoolV true -> UnitV
      | BoolV false -> err "Assertion failed"
      | _ -> assert false)
  | TypedExp (exp, _) -> eval_exp env exp

let eval_decl env = function
  | LetDecl (pat, expb) ->
    let vb = eval_exp env expb in
    let binds = match_pat vb pat in
    let env = List.fold_left (fun env (id, v) -> Env.extend id v env) env binds in
    (binds, env)
  | LetRecDecl (pat, expb) ->
    let vars = vars_pat pat in
    let env = List.fold_left (fun env id -> Env.reserve id env) env vars in
    let vb = eval_exp env expb in
    let binds = match_pat vb pat in
    List.iter (fun (id, v) -> Env.emplace id v env) binds;
    (binds, env)

let eval_program env = function
  | Exp e -> (["", eval_exp env e], env)
  | Decls decls ->
    List.fold_left
      (fun (vals, env) decl ->
        let vals', env = eval_decl env decl in
        (vals @ vals', env))
      ([], env) decls
