open OUnit
open Assertion
open Utils

let _ =
  "syntax" >::: [
    "comment" >::: [
      TestCase (fun _ -> assert_type "unit" "(* com *) () (* ment *)");
      TestCase (fun _ -> assert_type "unit" "(* (* nested *) comment *) ()");
      TestCase (fun _ -> assert_lexfail "(* (* unmatched *) comment ((()");
      TestCase (fun _ -> assert_type "unit" "(*) not an operator *) ()");
      TestCase (fun _ -> assert_type "int -> int -> int" "( *)");
    ];
    "operator_precedence" >::: [
      TestCase (fun _ -> assert_value "4" "1 + if false then 1 else 1 + 2");
      TestCase (fun _ -> assert_value "4" "1 + let x = 2 in x + 1");
      TestCase (fun _ -> assert_value "4" "1 + match 2 with a -> a + 1");
    ];
  ] |> run_test_tt
