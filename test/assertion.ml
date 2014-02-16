open OUnit

let assert_type typ exp =
  let result, _ = Interpret.interpretmany_string Prelude.env exp in
  let _, ty, _ = List.hd result in
  assert_equal typ (Typing.string_of_ty ty)

let assert_value vstr exp =
  let result, _ = Interpret.interpretmany_string Prelude.env exp in
  let _, _, v = List.hd result in
  assert_equal vstr (Eval.string_of_exval v)

let assert_lexfail exp =
  try
    ignore(Interpret.interpretmany_string Prelude.env exp);
    assert_failure "expected to fail"
  with
    | Interpret.Lex_error _ -> ()

let assert_parsefail exp =
  try
    ignore(Interpret.interpretmany_string Prelude.env exp);
    assert_failure "expected to fail"
  with
    | Interpret.Parse_error _ -> ()

let assert_semanticfail exp =
  try
    ignore(Interpret.interpretmany_string Prelude.env exp);
    assert_failure "expected to fail"
  with
    | Interpret.Semantic_error _ -> ()

let assert_typefail exp =
  try
    ignore(Interpret.interpretmany_string Prelude.env exp);
    assert_failure "expected to fail"
  with
    | Interpret.Type_error _ -> ()

let assert_evalfail exp =
  try
    ignore(Interpret.interpretmany_string Prelude.env exp);
    assert_failure "expected to fail"
  with
    | Interpret.Eval_error _ -> ()
