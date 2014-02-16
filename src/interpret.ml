exception Lex_error of string
exception Parse_error of string
exception Semantic_error of string
exception Type_error of string
exception Eval_error of string

let interpret_program (tyenv, venv) prog =
    let tys, tyenv = Typing.type_program tyenv prog in
    let vals, venv = Eval.eval_program venv prog in
    ((List.map2
        (fun (tyid, ty) (vid, v) ->
          assert (tyid = vid);
          (tyid, ty, v)) tys vals), (tyenv, venv))

let interpretone env lexbuf =
  try
    let prog = Parser.program Lexer.main lexbuf in
    Semantic.check_program prog;
    interpret_program env prog
  with
    | Failure msg -> raise (Lex_error msg)
    | Parsing.Parse_error -> raise (Parse_error "")
    | Semantic.Error msg -> raise (Semantic_error msg)
    | Typing.Error msg -> raise (Type_error msg)
    | Eval.Error msg -> raise (Eval_error msg)

let interpretone_channel env chan =
  interpretone env (Lexing.from_channel chan)

let interpretone_string env str =
  interpretone env (Lexing.from_string str)

let interpretmany env lexbuf =
  try
    let progs = Parser.programs Lexer.main lexbuf in
    List.fold_left
      (fun (results, env) prog ->
        Semantic.check_program prog;
        interpret_program env prog) ([], env) progs
  with
    | Failure msg -> raise (Lex_error msg)
    | Parsing.Parse_error -> raise (Parse_error "")
    | Semantic.Error msg -> raise (Semantic_error msg)
    | Typing.Error msg -> raise (Type_error msg)
    | Eval.Error msg -> raise (Eval_error msg)

let interpretmany_channel env chan =
  interpretmany env (Lexing.from_channel chan)

let interpretmany_string env str =
  interpretmany env (Lexing.from_string str)
