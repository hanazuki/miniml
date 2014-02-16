%{
open Syntax

let binop op lhs rhs = AppExp (AppExp (VarExp op, lhs), rhs)
%}

%token EOF
%token LBRACKET RBRACKET LPAREN RPAREN CONS COLON SEMI SEMISEMI COMMA RARROW BAR WILD
%token AMPAMP BARBAR PLUS MULT LESS EQUALS
%token AND AS ASSERT BOOL ELSE FALSE FUN IF IN INT LET MATCH REC THEN TRUE UNIT WHEN WITH

%token <int> INTV
%token <Syntax.id> ID TVID

%start program programs toplevel
%type <Syntax.program list> programs
%type <Syntax.program> program toplevel
%%

toplevel: program { $1 }

programs:
    EOF { [] }
  | Expr EOF { [Exp $1] }
  | Decls EOF { [Decls (List.rev $1)] }
  | Expr SEMISEMI programs { Exp $1 :: $3 }
  | Decls SEMISEMI programs { Decls (List.rev $1) :: $3 }

program:
    EOF { raise Exit }
  | Expr SEMISEMI { Exp $1 }
  | Decls SEMISEMI { Decls (List.rev $1) }

Id:
    ID { $1 }
  | LPAREN PLUS RPAREN { "+" }
  | LPAREN MULT RPAREN { "*" }
  | LPAREN EQUALS RPAREN { "=" }
  | LPAREN LESS RPAREN { "<" }
  | LPAREN BARBAR RPAREN { "||" }
  | LPAREN AMPAMP RPAREN { "&&" }

Decls :
    Decl { [$1] }
  | Decls Decl { $2 :: $1 }

Decl :
    LET LetBinds {
      let pats, exps = List.split (List.rev $2) in
      LetDecl (TuplePat pats, TupleExp exps)
    }
  | LET REC LetBinds {
      let pats, exps = List.split (List.rev $3) in
      LetRecDecl (TuplePat pats, TupleExp exps)
    }

LetBind:
    Patt EQUALS Expr { ($1, $3) }
  | Id PattsSp EQUALS Expr {
    (VarPat $1, List.fold_left (fun exp pat -> FunExp (pat, exp)) $4 $2)
  }

LetBinds :
    LetBind { [$1] }
  | LetBinds AND LetBind { $3 :: $1 }

Exprs:
    Expr { [$1] }
  | Exprs SEMI Expr { $3 :: $1 }

Expr:
    KeyExpr { $1 }
  | TupleExpr { TupleExp (List.rev $1) }
  | SeqOrExpr { $1 }

KeyExpr:
    IfExpr { $1 }
  | LetExpr { $1 }
  | FunExpr { $1 }
  | MatchExpr { $1 }

IfExpr:
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr:
    LET LetBinds IN Expr {
      let pats, exps = List.split (List.rev $2) in
      LetExp (TuplePat pats, TupleExp exps, $4)
    }
  | LET REC LetBinds IN Expr {
      let pats, exps = List.split (List.rev $3) in
      LetRecExp (TuplePat pats, TupleExp exps, $5)
    }

FunExpr:
    FUN PattsSp RARROW Expr {
      List.fold_left (fun exp pat -> FunExp (pat, exp)) $4 $2 
    }

MatchExpr:
    MATCH Expr WITH Matching { MatchExp ($2, List.rev $4) }
  | MATCH Expr WITH BAR Matching { MatchExp ($2, List.rev $5) }

TupleExpr:
    TupleExpr COMMA SeqOrExpr { $3 :: $1 }
  | SeqOrExpr COMMA SeqOrExpr { [$3; $1] }
  | SeqOrExpr COMMA KeyExpr { [$3; $1] }

SeqOrExpr:
    SeqOrExpr BARBAR SeqAndExpr { binop "||" $1 $3 }
  | SeqOrExpr BARBAR KeyExpr { binop "||" $1 $3 }
  | SeqAndExpr { $1 }

SeqAndExpr:
    SeqAndExpr AMPAMP LessExpr { binop "&&" $1 $3 }
  | SeqAndExpr AMPAMP KeyExpr { binop "&&" $1 $3 }
  | LessExpr { $1 }

LessExpr:
    ConsExpr EQUALS ConsExpr { binop "=" $1 $3 }
  | ConsExpr EQUALS KeyExpr { binop "=" $1 $3 }
  | ConsExpr LESS ConsExpr { binop "<" $1 $3 }
  | ConsExpr LESS KeyExpr { binop "<" $1 $3 }
  | ConsExpr { $1 }

ConsExpr:
    PlusExpr CONS ConsExpr { ConsExp ($1, $3) }
  | PlusExpr CONS KeyExpr { ConsExp ($1, $3) }
  | PlusExpr { $1 }

PlusExpr:
    PlusExpr PLUS MultExpr { binop "+" $1 $3 }
  | PlusExpr PLUS KeyExpr { binop "+" $1 $3 }
  | MultExpr { $1 }

MultExpr: 
    MultExpr MULT AppExpr { binop "*" $1 $3 }
  | MultExpr MULT KeyExpr { binop "*" $1 $3 }
  | AppExpr { $1 }

AppExpr:
    AppExpr PrimExpr { AppExp ($1, $2) }
  | ASSERT Expr { AssertExp $2 }
  | PrimExpr { $1 }

PrimExpr:
    INTV { ILitExp $1 }
  | TRUE { BLitExp true }
  | FALSE { BLitExp false }
  | ListExpr { $1 }
  | Id { VarExp $1 }
  | LPAREN RPAREN { UnitExp }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN Expr COLON TyExpr RPAREN { TypedExp ($2, $4) }

ListExpr:
    LBRACKET RBRACKET { ListExp [] }
  | LBRACKET Exprs RBRACKET { ListExp (List.rev $2) }
  | LBRACKET Exprs SEMI RBRACKET { ListExp (List.rev $2) }
  
Matching:
    Patt RARROW Expr { [$1, $3, BLitExp true] }
  | Patt WHEN Expr RARROW Expr { [$1, $5, $3] }
  | Matching BAR Patt RARROW Expr { ($3, $5, BLitExp true) :: $1 }
  | Matching BAR Patt WHEN Expr RARROW Expr { ($3, $7, $5) :: $1 }

TyExpr:
  | TyExpr AS TVID { NamedTyexp ($3, $1) }
  | FunTyExpr { $1 }

FunTyExpr:
    PrimTyExpr RARROW FunTyExpr { FunTyexp ($1, $3) }
  | TupleTyExpr { TupleTyexp (List.rev $1) }
  | PrimTyExpr { $1 }

TupleTyExpr:
    PrimTyExpr MULT PrimTyExpr { [$3; $1] }
  | TupleTyExpr MULT PrimTyExpr { $3 :: $1 }

PrimTyExpr:
    WILD { AnyTyexp }
  | UNIT { UnitTyexp }
  | INT { IntTyexp }
  | BOOL { BoolTyexp }
  | PrimTyExpr Id {
    match $2 with
      | "list" -> ListTyexp $1
      | x -> raise Parse_error
  }
  | TVID { VarTyexp $1 }
  | LPAREN TyExpr RPAREN { $2 }

PattsSc:
    Patt { [$1] }
  | PattsSc SEMI Patt { $3 :: $1 }

PattsSp:
    Patt { [$1] }
  | PattsSp Patt { $2 :: $1 }

Patt:
    AltPatt { $1 }
  | Patt AS Id { NamedPat ($3, $1) }

AltPatt:
    TuplePatt BAR AltPatt { AltPat (TuplePat (List.rev $1), $3) }
  | ConsPatt BAR AltPatt { AltPat ($1, $3) }
  | TuplePatt { TuplePat (List.rev $1) }
  | ConsPatt { $1 }

TuplePatt:
    TuplePatt COMMA ConsPatt { $3 :: $1 }
  | ConsPatt COMMA ConsPatt { [$3; $1] }

ConsPatt:
    PrimPatt CONS ConsPatt { ConsPat ($1, $3) }
  | PrimPatt { $1 }

PrimPatt:
    WILD { AnyPat }
  | Id { VarPat $1 }
  | INTV { ILitPat $1 }
  | TRUE { BLitPat true }
  | FALSE { BLitPat false }
  | ListPatt { $1 }
  | LPAREN RPAREN { UnitPat }
  | LPAREN Patt RPAREN { $2 }
  | LPAREN Patt COLON TyExpr RPAREN { TypedPat ($2, $4) }

ListPatt:
    LBRACKET RBRACKET { ListPat [] }
  | LBRACKET PattsSc RBRACKET { ListPat (List.rev $2) }
  | LBRACKET PattsSc SEMI RBRACKET { ListPat (List.rev $2) }
