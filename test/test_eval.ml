open OUnit
open Assertion
open Utils

let _ =
  "eval" >::: [
    "unit_literal" >::: [
      TestCase (fun _ -> assert_value "()" "()");
      TestCase (fun _ -> assert_value "()" "( )");
    ];
    "int_literal" >::: [
      TestCase (fun _ -> assert_value "0" "0");
      TestCase (fun _ -> assert_value "42" "42");
      TestCase (fun _ -> assert_value "-4" "-4");
    ];
    "bool_literal" >::: [
      TestCase (fun _ -> assert_value "true" "true");
      TestCase (fun _ -> assert_value "false" "false");
    ];
    "if" >::: [
      TestCase (fun _ -> assert_value "3" "if true then 3 else 4");
      TestCase (fun _ -> assert_value "4" "if false then 3 else 4");
    ];
    "let_decl" >::: [
      TestCase (fun _ -> assert_value "4" "let x = 4;; x");
      TestCase (fun _ -> assert_value "5" "let x = 4 let x = 5;; x");
      TestCase (fun _ -> assert_value "8" "let x = 4 and y = 8;; y");
      TestCase (fun _ -> assert_value "4" "let x = 4 let x = 5 and y = x;; y");
      "syntax" >::: [
        TestCase (fun _ -> assert_value "false" "let f x y = x < y;; f 5 4");
      ];
    ];
    "let_expr" >::: [
      TestCase (fun _ -> assert_value "4" "let x = 4 in x");
      TestCase (fun _ -> assert_value "8" "let x = 4 in let x = 8 in x");
      TestCase (fun _ -> assert_value "8" "let x = 4 and y = 8 in y");
      TestCase (fun _ -> assert_value "4" "let x = 4 in let x = 5 and y = x in y");
      "syntax" >::: [
        TestCase (fun _ -> assert_value "false" "let f x y = x < y in f 5 4");
      ];
    ];
    "letrec_decl" >::: [
      TestCase (fun _ -> assert_value "4" "let rec x = 4;; x");
      TestCase (fun _ -> assert_value "8" "let rec x = 4 let rec x = 8;; x");
      TestCase (fun _ -> assert_value "8" "let rec x = 4 and y = 8;; y");
      TestCase (fun _ -> assert_value "4" "let rec f = fun x -> if x < 4 then f (x + 1) else x;; f 0");
      TestCase (fun _ -> assert_value "15" "let rec f = fun x -> if x < 10 then g (x + -1) else x and g = fun x -> f (x * 3);; f 3");
      "syntax" >::: [
        TestCase (fun _ -> assert_value "4" "let rec f x = if x < 4 then f (x + 1) else x;; f 0");
        TestCase (fun _ -> assert_value "15" "let rec f x = if x < 10 then g (x + -1) else x and g x = f (x * 3);; f 3");
      ]
    ];
    "letrec_expr" >::: [
      TestCase (fun _ -> assert_value "4" "let rec x = 4 in x");
      TestCase (fun _ -> assert_value "8" "let rec x = 4 in let rec x = 8 in x");
      TestCase (fun _ -> assert_value "8" "let rec x = 4 and y = 8 in y");
      TestCase (fun _ -> assert_value "4" "let rec f = fun x -> if x < 4 then f (x + 1) else x in f 0");
      TestCase (fun _ -> assert_value "15" "let rec f = fun x -> if x < 10 then g (x + -1) else x and g = fun x -> f (x * 3) in f 3");
    ];
    "function" >::: [
      TestCase (fun _ -> assert_value "4" "(fun x -> 4) 0");
      TestCase (fun _ -> assert_value "8" "(fun x -> x) 8");
      TestCase (fun _ -> assert_value "8" "(fun x -> fun y -> x) 8 4");
      TestCase (fun _ -> assert_value "8" "(fun x -> x) (fun x -> x * 2) 4");
      "syntax" >::: [
        TestCase (fun _ -> assert_value "false" "(fun x y -> x < y) 5 4");
      ];
    ];
    "assert_expression" >::: [
      TestCase (fun _ -> assert_value "()" "assert true");
      TestCase (fun _ -> assert_evalfail "assert false");
    ];
    "prelude" >::: [
      "bool_and" >::: [
        TestCase (fun _ -> assert_value "true" "true && true");
        TestCase (fun _ -> assert_value "false" "true && false");
        TestCase (fun _ -> assert_value "false" "false && true");
        TestCase (fun _ -> assert_value "false" "false && false");
      ];
      "bool_or" >::: [
        TestCase (fun _ -> assert_value "true" "true || true");
        TestCase (fun _ -> assert_value "true" "true || false");
        TestCase (fun _ -> assert_value "true" "false || true");
        TestCase (fun _ -> assert_value "false" "false || false");
      ];
      "int_add" >::: [
        TestCase (fun _ -> assert_value "18" "15 + 3");
        TestCase (fun _ -> assert_value "12" "15 + -3");
      ];
      "int_mul" >::: [
        TestCase (fun _ -> assert_value "45" "15 * 3");
        TestCase (fun _ -> assert_value "-45" "15 * -3");
      ];
      "eq" >::: [
        TestCase (fun _ -> assert_value "true" "() = ()");
        TestCase (fun _ -> assert_value "false" "1 = 2");
        TestCase (fun _ -> assert_value "true" "1 = 1");
        TestCase (fun _ -> assert_value "true" "false = false");
        TestCase (fun _ -> assert_value "false" "false = true");
        TestCase (fun _ -> assert_value "false" "true = false");
        TestCase (fun _ -> assert_value "true" "true = true");
        TestCase (fun _ -> assert_value "true" "[] = []");
        TestCase (fun _ -> assert_value "false" "[] = [1]");
        TestCase (fun _ -> assert_value "false" "[1] = []");
        TestCase (fun _ -> assert_value "true" "[1] = [1]");
        TestCase (fun _ -> assert_value "false" "[1] = [2]");        
        TestCase (fun _ -> assert_evalfail "(=) = (=)");
        TestCase (fun _ -> assert_evalfail "(fun x -> x) = (fun x -> x)");
      ];
      "lt" >::: [
        TestCase (fun _ -> assert_value "false" "() < ()");
        TestCase (fun _ -> assert_value "false" "15 < 3");
        TestCase (fun _ -> assert_value "false" "15 < -3");
        TestCase (fun _ -> assert_value "true" "3 <15");
        TestCase (fun _ -> assert_value "false" "false < false");
        TestCase (fun _ -> assert_value "true" "false < true");
        TestCase (fun _ -> assert_value "false" "true < false");
        TestCase (fun _ -> assert_value "false" "true < true");
        TestCase (fun _ -> assert_value "false" "[] < []");
        TestCase (fun _ -> assert_value "true" "[] < [1]");
        TestCase (fun _ -> assert_value "false" "[1] < []");
        TestCase (fun _ -> assert_value "false" "[1] < [1]");
        TestCase (fun _ -> assert_value "true" "[1] < [2]");
        TestCase (fun _ -> assert_value "false" "[2] < [1]");
        TestCase (fun _ -> assert_evalfail "(<) < (=)");
        TestCase (fun _ -> assert_evalfail "(fun x -> x) < (fun x -> x)");
      ];
    ];
    "operator_as_function" >::: [
      TestCase (fun _ -> assert_value "6" "(+) 2 4");
      TestCase (fun _ -> assert_value "6" "let addtwo = (+) 2 in addtwo 4");
      TestCase (fun _ -> assert_value "20" "(fun f x -> f (f x x) (f x x)) (+) 5");
      TestCase (fun _ -> assert_value "20" "( *) 4 5");
      TestCase (fun _ -> assert_value "true" "(<) 4 5");
      TestCase (fun _ -> assert_value "true" "(||) false true");
      TestCase (fun _ -> assert_value "false" "(&&) true false");
    ];
    "list" >::: [
      "literal" >::: [
        TestCase (fun _ -> assert_value "[]" "[]");
        TestCase (fun _ -> assert_value "[1]" "[1]");
        TestCase (fun _ -> assert_value "[1; 2]" "[1; 2]");
        TestCase (fun _ -> assert_value "[1; 3]" "[1; 3;]");
      ];
      "cons" >::: [
        TestCase (fun _ -> assert_value "[1]" "1 :: []");
        TestCase (fun _ -> assert_value "[1; 2]" "1 :: [2]");
        TestCase (fun _ -> assert_value "[1; 2; 3; 4; 5]" "1 :: 2 :: 3 :: [4; 5]");
      ]
    ];
    "tuple" >::: [
      TestCase (fun _ -> assert_value "(1, 2)" "1, 2");
      TestCase (fun _ -> assert_value "(1, 2, 3)" "1, 2, 3");
      TestCase (fun _ -> assert_value "(1, (2, 3))" "1, (2, 3)");
      TestCase (fun _ -> assert_value "(1, [2; 3])" "1, [2; 3]");
    ];
    "match" >::: [
      TestCase (fun _ -> assert_value "8" "match 5 with 5 -> 8");
      TestCase (fun _ -> assert_evalfail "match 5 with 4 -> 8");
      TestCase (fun _ -> assert_value "6" "match 5 with 4 -> 8 | 5 -> 6");
      TestCase (fun _ -> assert_value "8" "match 5 with 5 -> 8 | 5 -> 6");
      TestCase (fun _ -> assert_value "8" "match 5 with a when a < 6 -> 8");
      TestCase (fun _ -> assert_value "8" "match 10 with a when a < 6 -> 4 | b -> 8");
    ];
    "pattern" >::: [
      TestCase (fun _ -> assert_value "()" "let _ = [1; 2], (3, 4) in ()");
      TestCase (fun _ -> assert_value "()" "let _, _ = [1; 2], (3, 4) in ()");
      TestCase (fun _ -> assert_value "()" "let (_ :: [_]), (_, _) = [1; 2], (3, 4) in ()");
      TestCase (fun _ -> assert_value "(4, [3; 2])" "let a :: b = [4; 3; 2] in (a, b)");
      TestCase (fun _ -> assert_value "(1, 2, 3)" "let [a; b; c] = [1; 2; 3] in (a, b, c)");
      TestCase (fun _ -> assert_value "3" "let ([4; a]|[a; 4]) = [3; 4] in a");
      TestCase (fun _ -> assert_value "3" "let [_; _ as a] = [2; 3] in a");
    ];
  ] |> run_test_tt
