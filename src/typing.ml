open Syntax
open Utils

exception Error of string
let err s = raise (Error s)

module TyVar = struct
  type t = int
  let compare = compare

  (** creates an unique type variable *)
  let create =
    let counter = ref 0 in
    fun () ->
      let v = !counter in
      incr counter; v
end

module TyVarMap = struct
  include Map.Make(TyVar)

  (* for OCaml < 3.12.0 *)
  let cardinal m = fold (fun _ _ n -> n + 1) m 0
  let bindings m = fold (fun k v x -> (k, v) :: x) m []
end

module TyVarSet = Set.Make(TyVar)

type ty =
  | VarT of TyVar.t
  | UnitT
  | IntT
  | BoolT
  | TupleT of ty list
  | ListT of ty
  | FunT of ty * ty

type tysc = TyVarSet.t * ty

type env = tysc Env.t

(** print a type
    @param ty type to print
*)
let type_stringizer () =
  let vars = ref TyVarMap.empty in
  let buffer = Buffer.create 100 in
  let rec str priority = function
    | VarT tv ->
      let s = (try TyVarMap.find tv !vars with
        | Not_found ->
          let i = TyVarMap.cardinal !vars in
          let s = String.make 1 (Char.chr ((Char.code 'a') + i mod 26)) ^
            if i < 26 then "" else string_of_int (i / 26)
          in
          vars := TyVarMap.add tv s !vars; s)
      in
      Buffer.add_char buffer '\'';
      Buffer.add_string buffer s;
    | UnitT -> Buffer.add_string buffer "unit"
    | IntT -> Buffer.add_string buffer "int"
    | BoolT -> Buffer.add_string buffer "bool"
    | TupleT tys ->
      (if priority > 1 then Buffer.add_char buffer '(');
      str 2 (List.hd tys);
      List.iter (fun ty -> Buffer.add_string buffer " * "; str 2 ty) (List.tl tys);
      (if priority > 1 then Buffer.add_char buffer ')')
    | ListT tye ->
      (if priority > 2 then Buffer.add_char buffer '(');
      str 2 tye;
      Buffer.add_string buffer " list";
      (if priority > 2 then Buffer.add_char buffer ')')
    | FunT (tya, tyr) ->
      (if priority > 0 then Buffer.add_char buffer '(');
      str 1 tya;
      Buffer.add_string buffer " -> ";
      str 0 tyr;
      (if priority > 0 then Buffer.add_char buffer ')')
  in
  fun ty ->
    Buffer.clear buffer;
    str 0 ty;
    Buffer.contents buffer

let string_of_ty ty = type_stringizer () ty
let print_ty = print_string $ string_of_ty

(** gets the set of free variables in a type
    @return set of free variables
*)
let rec freevars_ty = function
  | VarT v -> TyVarSet.singleton v
  | UnitT
  | IntT
  | BoolT -> TyVarSet.empty
  | TupleT tys ->
    List.fold_left (fun vars ty -> TyVarSet.union vars (freevars_ty ty)) TyVarSet.empty tys
  | ListT tye -> freevars_ty tye
  | FunT (tya, tyr) -> TyVarSet.union (freevars_ty tya) (freevars_ty tyr)

let freevars_tysc (tvs, ty) = (TyVarSet.diff (freevars_ty ty) tvs)

let freevars_env env =
  Env.fold (fun _ -> TyVarSet.union $ freevars_tysc) env TyVarSet.empty

(** type substitutions
*)
module Subst = struct
  type t = ty TyVarMap.t

  let identity = TyVarMap.empty

  let add tv ty =
    let rec subst = function
      | VarT tv' -> if tv' = tv then ty else VarT tv'
      | UnitT -> UnitT
      | IntT -> IntT
      | BoolT -> BoolT
      | TupleT ts -> TupleT (List.map subst ts)
      | ListT e -> ListT (subst e)
      | FunT (a, r) -> FunT (subst a, subst r)
    in
    TyVarMap.add tv ty $ TyVarMap.map subst

  let concat s s' =
    TyVarMap.fold add s' s

  let rec apply s = function
    | VarT tv as ty -> (try TyVarMap.find tv s with Not_found -> ty)
    | UnitT -> UnitT
    | IntT -> IntT
    | BoolT -> BoolT
    | TupleT ts -> TupleT (List.map (apply s) ts)
    | ListT e -> ListT (apply s e)
    | FunT (a, r) -> FunT (apply s a, apply s r)

  let equations =
    (List.rev_map (fun (tv, ty) -> VarT tv, ty)) $ TyVarMap.bindings
end

(* for debug *)
let print_eqs, print_subst =
  let str = type_stringizer () in
  (fun eqs -> List.iter (fun (tyl, tyr) ->
    print_endline (str tyl ^ " == " ^ str tyr)) eqs),
  (fun s -> List.iter (fun (tyl, tyr) ->
    print_endline (str tyl ^ " <- " ^ str tyr)) (Subst.equations s))

(** unify types
    @return substitutions performed in unification
*)
let unify =
  let rec unify_aux s (tyl, tyr) =
    match Subst.apply s tyl, Subst.apply s tyr with
      | tyl, tyr when tyl = tyr -> s
      | VarT tv, ty | ty, VarT tv ->
        if TyVarSet.mem tv (freevars_ty ty) then err "Recursive type detected." else
          Subst.add tv ty s
      | TupleT tys, TupleT tys' when List.length tys = List.length tys' ->
        List.fold_left unify_aux s (List.combine tys tys')
      | ListT ty, ListT ty' ->
        unify_aux s (ty, ty')
      | FunT (tya, tyr), FunT (tya', tyr') ->
        List.fold_left unify_aux s [tya, tya'; tyr, tyr']
      | tyl, tyr ->
        let str = type_stringizer () in
        err ("Cannot unify types: " ^ (str tyl) ^ " and " ^ (str tyr))
  in
  List.fold_left unify_aux Subst.identity

let closure ty env s =
  let fv_env =
    (TyVarSet.fold
       (fun tv -> TyVarSet.union (freevars_ty (Subst.apply s (VarT tv))))
       (freevars_env env) TyVarSet.empty)
  and fv_ty = freevars_ty (Subst.apply s ty) in
  (TyVarSet.diff fv_ty fv_env, ty)

(** evaluates type expression
    @param tyexp type expression to evaluate
    @return type equations that should be satisfied and the type
*)
let eval_tyexp tyexp =
  let tvm = ref IdMap.empty in
  let gettv id =
    try IdMap.find id !tvm with
      | Not_found ->
        let tv = TyVar.create () in
        tvm := IdMap.add id tv !tvm; tv
  in
  let rec eval_tyexp_aux = function
    | AnyTyexp -> ([], VarT (TyVar.create ()))
    | VarTyexp id -> ([], VarT (gettv id))
    | UnitTyexp -> ([], UnitT)
    | IntTyexp -> ([], IntT)
    | BoolTyexp -> ([], BoolT)
    | TupleTyexp tyexps ->
      let eqs, tys = List.split (List.map eval_tyexp_aux tyexps) in
      (List.flatten eqs, TupleT tys)
    | ListTyexp tyexp ->
      let eq, ty = eval_tyexp_aux tyexp in
      (eq, ListT ty)
    | FunTyexp (tyexpa, tyexpr) ->
      let eqa, tya = eval_tyexp_aux tyexpa
      and eqr, tyr = eval_tyexp_aux tyexpr in
      (eqa @ eqr, FunT (tya, tyr))
    | NamedTyexp (id, tyexp) ->
      let eq, ty = eval_tyexp_aux tyexp in
      ((VarT (gettv id), ty) :: eq, ty)
  in
  eval_tyexp_aux tyexp

(** matches a type against specified pattern
   @return
*)
let rec match_pat ty = function
  | AnyPat -> ([], [])
  | VarPat id -> ([], [id, ty])
  | UnitPat -> ([ty, UnitT], [])
  | ILitPat _ -> ([ty, IntT], [])
  | BLitPat _ -> ([ty, BoolT], [])
  | TuplePat ps ->
    let tvm = List.map (fun p -> p, TyVar.create ()) ps in
    let tyt = TupleT (List.map (fun tv -> VarT tv) (snd (List.split tvm))) in
    List.fold_left (fun (eqs, binds) (p, tv) ->
      let eqs', binds' = match_pat (VarT tv) p in
      (eqs @ eqs', binds @ binds')) ([ty, tyt], []) tvm
  | ListPat ps ->
    let tve = TyVar.create () in
    List.fold_left (fun (eqs, binds) p ->
      let eqs', binds' = match_pat (VarT tve) p in
      (eqs @ eqs', binds @ binds')) ([ty, ListT (VarT tve)], []) ps
  | ConsPat (ph, pt) ->
    let tve = TyVar.create () in
    let eqsh, bindsh = match_pat (VarT tve) ph
    and eqst, bindst = match_pat (ListT (VarT tve)) pt in
    ((ty, ListT (VarT tve)) :: eqsh @ eqst, bindsh @ bindst)
  | AltPat (pl, pr) ->
    let eqsl, bindsl = match_pat ty pl
    and eqsr, bindsr = match_pat ty pr in
    let eqsb = List.rev_map (fun (id, tyl) -> (tyl, List.assoc id bindsr)) bindsl in
    (eqsl @ eqsr @ eqsb, bindsl)
  | NamedPat (id, p) ->
    let eqs, binds = match_pat ty p in
    (eqs, binds @ [id, ty])
  | TypedPat (pat, tyexp) ->
    let eqs, binds = match_pat ty pat in
    let eqc, tyc = eval_tyexp tyexp in
    ([ty, tyc] @ eqc @ eqs, binds)

(** type an expresssion
    @param env environment
*)
let rec type_exp env = function
  | VarExp id ->
    (try
       let tvs, ty = Env.lookup id env in
       let s = TyVarSet.fold (fun tv -> Subst.add tv (VarT (TyVar.create ()))) tvs Subst.identity in
       (Subst.identity, Subst.apply s ty)
     with
       | Env.Not_bound -> err ("Variable not bound: " ^ id))
  | UnitExp -> (Subst.identity, UnitT)
  | ILitExp _ -> (Subst.identity, IntT)
  | BLitExp _ -> (Subst.identity, BoolT)
  | TupleExp exps ->
    let ss, tys = List.split (List.map (type_exp env) exps) in
    let eqs = List.flatten (List.rev_map Subst.equations ss) in
    let s = unify eqs in
    (s, Subst.apply s (TupleT tys))
  | ListExp exps ->
    let tve = TyVar.create () in
    let ss, tys = List.split (List.rev_map (type_exp env) exps) in
    let eqs = List.map (fun ty -> (VarT tve, ty)) tys @ List.flatten (List.rev_map Subst.equations ss) in
    let s = unify eqs in
    (s, Subst.apply s (ListT (VarT tve)))
  | ConsExp (exph, expt) ->
    let sh, tyh = type_exp env exph
    and st, tyt = type_exp env expt in
    let eqs = [ListT tyh, tyt] @ Subst.equations sh @ Subst.equations st in
    let s = unify eqs in
    (s, Subst.apply s tyt)
  | IfExp (expt, exp1, exp0) ->
    let st, tyt = type_exp env expt
    and s1, ty1 = type_exp env exp1
    and s0, ty0 = type_exp env exp0 in
    let eqs = [tyt, BoolT; ty1, ty0] @ Subst.equations st @ Subst.equations s1 @ Subst.equations s0 in
    let s = unify eqs in
    (s, Subst.apply s ty1)
  | LetExp (pat, expb, expe) ->
    let sb, tyb = type_exp env expb in
    let eq, binds = match_pat tyb pat in
    let s = unify (Subst.equations sb @ eq) in
    let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
    let se, tye = type_exp env expe in
    let s = unify (Subst.equations s @ Subst.equations se) in
    (s, Subst.apply s tye)
  | LetRecExp (pat, expb, expe) ->
    let tvb = TyVar.create () in
    let eq, binds = match_pat (VarT tvb) pat in
    let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
    let sb, tyb = type_exp env expb in
    let se, tye = type_exp env expe in
    let eqs = (VarT tvb, tyb) :: Subst.equations sb @ eq @ Subst.equations se in
    let s = unify eqs in
    (s, Subst.apply s tye)
  | FunExp (pat, exp) ->
    let tvp = TyVar.create () in
    let eq, binds = match_pat (VarT tvp) pat in
    let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
    let se, tye = type_exp env exp in
    let eqs = eq @ Subst.equations se in
    let s = unify eqs in
    (s, Subst.apply s (FunT (VarT tvp, tye)))
  | AppExp (expf, expa) ->
    let tvr = TyVar.create () in
    let sf, tyf = type_exp env expf
    and sa, tya = type_exp env expa in
    let s = unify ([FunT (tya, VarT tvr), tyf] @ Subst.equations sf @ Subst.equations sa) in
    (s, Subst.apply s (VarT tvr))
  | MatchExp (exp, matchings) ->
    let tvr = TyVar.create () in
    let st, tyt = type_exp env exp in
    let eqs = List.flatten
      (List.map (fun (pat, exp, grd) ->
        let eqs, binds = match_pat tyt pat in
        let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
        let sg, tyg = type_exp env grd
        and se, tye = type_exp env exp in
        [tyg, BoolT; tye, VarT tvr] @ eqs @ Subst.equations sg @ Subst.equations se) matchings)
    in
    let s = unify eqs in
    (s, Subst.apply s (VarT tvr))
  | AssertExp exp ->
    let se, tye = type_exp env exp in
    let s = unify ((BoolT, tye) :: Subst.equations se) in
    (s, UnitT)
  | TypedExp (exp, tyexp) ->
    let se, tye = type_exp env exp in
    let eqc, tyc = eval_tyexp tyexp in
    let s = unify ((tye, tyc) :: eqc @ Subst.equations se) in
    (s, Subst.apply s tye)

(** type a declaration
    @param env Environment
*)
let type_decl env = function
  | LetDecl (pat, expb) ->
    let sb, tyb = type_exp env expb in
    let eq, binds = match_pat tyb pat in
    let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
    let eqs = Subst.equations sb @ eq in
    let s = unify eqs in
    List.map (fun (id, ty) -> (id, Subst.apply s ty)) binds, env
  | LetRecDecl (pat, expb) ->
    let tvb = TyVar.create () in
    let eq, binds = match_pat (VarT tvb) pat in
    let env = List.fold_left (fun env (id, ty) -> Env.extend id (TyVarSet.empty, ty) env) env binds in
    let sb, tyb = type_exp env expb in
    let eqs = (VarT tvb, tyb) :: Subst.equations sb @ eq in
    let s = unify eqs in
    List.map (fun (id, ty) -> (id, Subst.apply s ty)) binds, env

let type_program env = function
  | Exp e -> (["", snd (type_exp env e)], env)
  | Decls decls ->
    List.fold_left
      (fun (tys, env) decl ->
        let tys', env = type_decl env decl in
        (tys @ tys', env))
      ([], env) decls
