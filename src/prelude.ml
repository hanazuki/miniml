open Typing
open Eval
open Utils

let monad f = MonadV f
let dyad f = MonadV (monad $ f)

let monadT a r = FunT (a, r)
let dyadT a b r = FunT (a, monadT b r)

let varT c =
  let tv = TyVar.create () in
  (TyVarSet.singleton tv, c (VarT tv))

let sor = dyad
  (fun lhs rhs ->
    match lhs, rhs with
      | lazy (BoolV true), _ -> BoolV true
      | lazy (BoolV false), lazy (BoolV x) -> BoolV x
      | _, _ -> assert false)

let sand = dyad
  (fun lhs rhs ->
    match lhs, rhs with
      | lazy (BoolV false), _ -> BoolV false
      | lazy (BoolV true), lazy (BoolV x) -> BoolV x
      | _, _ -> assert false)

let iadd = dyad
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | IntV l, IntV r -> IntV (l + r)
      | _, _ -> assert false)

let imul = dyad
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | IntV l, IntV r -> IntV (l * r)
      | _, _ -> assert false)

let eq = dyad
  (fun (lazy lhs) (lazy rhs) ->
    let rec eq l r =
      match l, r with
        | UnitV, UnitV -> true
        | IntV l, IntV r -> l = r
        | BoolV l, BoolV r -> l = r
        | (FunV _ | MonadV _), (FunV _ | MonadV _) ->
          err "Functional values are not comparable: ="
        | ListV l, ListV r ->
          (try
             List.fold_left2 (fun b l r -> b && (eq l r)) true l r
           with
             | Invalid_argument _ -> false)
        | _, _ -> assert false
    in
    BoolV (eq lhs rhs))

let lt = dyad
  (fun (lazy lhs) (lazy rhs) ->
    let rec lt l r =
      match l, r with
        | UnitV, UnitV -> false
        | IntV l, IntV r -> l < r
        | BoolV l, BoolV r -> l < r
        | (FunV _ | MonadV _), (FunV _ | MonadV _) ->
          err "Functional values are not comparable: <"
        | ListV [], ListV [] -> false
        | ListV [], ListV _ -> true
        | ListV _, ListV [] -> false
        | ListV (l :: ls), ListV (r :: rs) -> (lt l r) || (lt (ListV ls) (ListV rs))
        | _, _ -> assert false
    in
    BoolV (lt lhs rhs))

let exit = monad
  (fun (lazy arg) ->
    match arg with
      | IntV i -> exit i
      | _ -> err "Argument must be int: exit")

let env =
  List.fold_left
    (fun (tyenv, venv) (id, ty, v) ->
      (Env.extend id ty tyenv, Env.extend id v venv))
    (Env.empty, Env.empty)
    ["||", (TyVarSet.empty, dyadT BoolT BoolT BoolT), sor;
     "&&", (TyVarSet.empty, dyadT BoolT BoolT BoolT), sand;
     "+", (TyVarSet.empty, dyadT IntT IntT IntT), iadd;
     "*", (TyVarSet.empty, dyadT IntT IntT IntT), imul;
     "<", varT (fun tv -> dyadT tv tv BoolT), lt;
     "=", varT (fun tv -> dyadT tv tv BoolT), eq;
     "exit", (TyVarSet.empty, monadT IntT UnitT), exit]
