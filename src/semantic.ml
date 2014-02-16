open Syntax
open Utils

exception Error of string

let rec check_exp = function
  | TypedExp (exp, _) ->
    check_exp exp
  | VarExp _
  | UnitExp
  | ILitExp _
  | BLitExp _ -> ()
  | TupleExp exps ->
    List.iter check_exp exps
  | ListExp exps ->
    List.iter check_exp exps
  | ConsExp (exp1, exp2) ->
    check_exp exp1;
    check_exp exp2
  | IfExp (exp1, exp2, exp3) ->
    check_exp exp1;
    check_exp exp2;
    check_exp exp3
  | LetExp (pat, exp0, exp1)
  | LetRecExp (pat, exp0, exp1) ->
    ignore (check_pat pat);
    check_exp exp0;
    check_exp exp1
  | FunExp (pat, exp) ->
    ignore (check_pat pat);
    check_exp exp
  | AppExp (exp1, exp2) ->
    check_exp exp1;
    check_exp exp2
  | MatchExp (exp, matchings) ->
    check_exp exp;
    List.iter
      (fun (pattern, exp1, exp2) ->
        check_exp exp1;
        check_exp exp2;
        ignore (check_pat pattern)) matchings
  | AssertExp exp ->
    check_exp exp
and check_pat = function
  | TypedPat (pat, _) ->
    check_pat pat
  | AnyPat -> IdSet.empty
  | VarPat id -> IdSet.singleton id
  | UnitPat -> IdSet.empty
  | ILitPat _ -> IdSet.empty
  | BLitPat _ -> IdSet.empty
  | TuplePat [] -> IdSet.empty
  | TuplePat (pattern :: patterns) ->
    let ids0 = check_pat pattern
    and ids1 = check_pat (ListPat patterns) in
    let dups = IdSet.inter ids0 ids1 in
    if IdSet.is_empty dups then
      IdSet.union ids0 ids1
    else
      raise (Error ("variable `" ^ IdSet.choose dups ^ "' is bound more than once in a matching"))
  | ListPat [] -> IdSet.empty
  | ListPat (pattern :: patterns) ->
    let ids0 = check_pat pattern
    and ids1 = check_pat (ListPat patterns) in
    let dups = IdSet.inter ids0 ids1 in
    if IdSet.is_empty dups then
      IdSet.union ids0 ids1
    else
      raise (Error ("variable `" ^ IdSet.choose dups ^ "' is bound more than once in a matching"))
  | ConsPat (pat1, pat2) ->
    let ids0 = check_pat pat1
    and ids1 = check_pat pat2 in
    let dups = IdSet.inter ids0 ids1 in
    if IdSet.is_empty dups then
      IdSet.union ids0 ids1
    else
      raise (Error ("variable `" ^ IdSet.choose dups ^ "' is bound more than once in a matching"))
  | AltPat (pat1, pat2) ->
    let ids0 = check_pat pat1
    and ids1 = check_pat pat2 in
    if IdSet.equal ids0 ids1 then
      ids1
    else
      raise (Error "variable sets of both sides of | pattern must be same")
  | NamedPat (name, pat) ->
    let ids = check_pat pat in
    if IdSet.mem name ids then
      raise (Error ("variable `" ^ name ^ "' is bound more than once in a matching"))
    else
      IdSet.add name ids

let check_decl = function
  | LetDecl (pat, exp)
  | LetRecDecl (pat, exp) ->
    ignore (check_pat pat);
    check_exp exp
    
let check_program = function
  | Exp exp -> check_exp exp
  | Decls decls -> List.iter check_decl decls
