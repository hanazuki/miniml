open OUnit
open Assertion
open Utils

let _ =
  "typing" >::: [
    "primitive" >::: [
      TestCase (fun _ -> assert_type "unit" "()");
      TestCase (fun _ -> assert_type "int" "42");
      TestCase (fun _ -> assert_type "bool" "true");
      TestCase (fun _ -> assert_type "bool" "false");
    ];
    "tuple" >::: [
      TestCase (fun _ -> assert_type "int * unit * bool" "(1, (), false)");
    ];
    "function" >::: [
      TestCase (fun _ -> assert_type "int" "1 + 1");
      TestCase (fun _ -> assert_type "'a -> 'a" "fun x -> x");
      TestCase (fun _ -> assert_type "'a -> int" "fun x -> 1");
      TestCase (fun _ -> assert_type "int -> int" "fun x -> x + 1");
      TestCase (fun _ -> assert_type "int -> int -> int" "(+)");
    ];
    "list" >::: [
      TestCase (fun _ -> assert_type "'a list" "[]");
      TestCase (fun _ -> assert_type "int list" "[1]");
      TestCase (fun _ -> assert_type "bool list" "[true]");
    ];
    "coercion" >::: [
      TestCase (fun _ -> assert_type "unit" "(() : unit)");
      TestCase (fun _ -> assert_type "unit" "(() : 'a)");
      TestCase (fun _ -> assert_typefail "(1 : unit)");
      TestCase (fun _ -> assert_type "unit list" "([()] : unit list)");
      TestCase (fun _ -> assert_typefail "([1] : unit list)");
      TestCase (fun _ -> assert_type "'a list" "([] : 'a list)");
      TestCase (fun _ -> assert_type "unit list" "([] : unit list)");
      TestCase (fun _ -> assert_type "bool -> bool" "(fun x -> x : 'a -> bool)");
    ];
    "testcases_0" >::: [
      TestCase (fun _ -> assert_type "int" "1 + 2");
      TestCase (fun _ -> assert_type "int" "-2 * 2");
      TestCase (fun _ -> assert_type "bool" "1 < 2");
      TestCase (fun _ -> assert_type "'a -> 'a" "fun x -> x");
      TestCase (fun _ -> assert_type "'a -> 'b -> 'a" "fun x -> fun y -> x");
      TestCase (fun _ -> assert_type "'a -> 'b -> 'b" "fun x -> fun y -> y");
      TestCase (fun _ -> assert_type "int" "(fun x -> x + 1) 2 + (fun x -> x + -1) 3");
      TestCase (fun _ -> assert_type "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c" "fun f -> fun g -> fun x -> g (f x)");
      TestCase (fun _ -> assert_type "('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c" "fun x -> fun y -> fun z -> x z (y z)");
      TestCase (fun _ -> assert_type "int -> int" "fun x -> let y = x + 1 in x");
      TestCase (fun _ -> assert_type "int -> int" "fun x -> let y = x + 1 in y");
      TestCase (fun _ -> assert_type "bool -> (bool -> bool) -> bool -> bool" "fun b -> fun x -> if x b then x else (fun x -> b)");
      TestCase (fun _ -> assert_type "bool -> bool" "fun x -> if true then x else (if x then true else false)");
      TestCase (fun _ -> assert_type "bool -> bool -> bool" "fun x -> fun y -> if x then x else y");
      TestCase (fun _ -> assert_type "'a -> 'a" "fun n -> (fun x -> x (fun y -> y)) (fun f -> f n)");
      TestCase (fun _ -> assert_type "('a -> 'b) -> 'a -> 'b" "fun x -> fun y -> x y");
      TestCase (fun _ -> assert_type "('a -> 'b) -> (('a -> 'b) -> 'a) -> 'b" "fun x -> fun y -> x (y x)");
      TestCase (fun _ -> assert_type "('a -> 'a -> 'b) -> (('a -> 'a -> 'b) -> 'a) -> 'b" "fun x -> fun y -> x (y x) (y x)");
      TestCase (fun _ -> assert_type "((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> (((('a -> 'b) -> 'a) -> 'b -> 'c) -> ('a -> 'b) -> 'a) -> 'c" "fun x -> fun y -> fun z -> x (z x) (y (z x y))");
      TestCase (fun _ -> assert_type "(('a -> 'a) -> 'a) -> 'a" "let id = fun x -> x in let f = fun y -> id (y id) in f");
      TestCase (fun _ -> assert_type "(('a -> 'b -> 'a) -> 'a) -> 'c -> 'b -> 'a" "let k = fun x -> fun y -> x in let k1 = fun x -> fun y -> k (x k) in k1");
      TestCase (fun _ -> assert_type "((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e -> 'f) -> ((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd -> 'e) -> ((('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c) -> 'd) -> 'f" "let s = fun x -> fun y -> fun z -> x z (y z) in let s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s)) in s1");
      TestCase (fun _ -> assert_type "'a -> (('a -> 'b -> 'c) -> 'd -> 'b) -> ('a -> 'b -> 'c) -> 'd -> 'c" "let g = fun h -> fun t -> fun f -> fun x -> f h (t f x) in g");
      TestCase (fun _ -> assert_type "'a -> 'a" "let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in let k' = fun x -> fun y -> x in s k k'");
      TestCase (fun _ -> assert_typefail "let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in s k k");
      TestCase (fun _ -> assert_typefail "let s = fun x -> fun y -> fun z -> x z (y z) in let k' = fun x -> fun y -> y in s k' k'");
      TestCase (fun _ -> assert_type "('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a" "fun x -> fun y -> fun z -> let b = x y z in if b then z y else y");
      TestCase (fun _ -> assert_type "int" "let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (pair 1 100)");
      TestCase (fun _ -> assert_typefail "let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)))");
      TestCase (fun _ -> assert_typefail "let f = fun x -> x in if f true then f 1 else f 2");
      TestCase (fun _ -> assert_typefail "let f = fun x -> 3 in f true + f 4");
      TestCase (fun _ -> assert_typefail "fun b -> let f = fun x -> x in let g = fun y -> y in if b then f g else g f");
      TestCase (fun _ -> assert_typefail "fun b -> fun f -> let g1 = fun x -> x f in let g2 = fun x -> x f in fun z -> if b then g1 z g2 else g2 z g1;;");
      TestCase (fun _ -> assert_typefail "1 + true");
      TestCase (fun _ -> assert_typefail "2 + (fun x -> x)");
      TestCase (fun _ -> assert_typefail "-2 * false");
      TestCase (fun _ -> assert_typefail "fun x -> x x");
      TestCase (fun _ -> assert_typefail "let f = fun x -> fun g -> g (x x g) in f f");
      TestCase (fun _ -> assert_typefail "let g = fun f -> fun x -> f x (f x) in g");
      TestCase (fun _ -> assert_typefail "let g = fun f -> fun x -> f x (x f) in g");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> x y + y x");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> x y + x");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> if x y then x else y");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> if x y then (fun z -> if y z then z else x) else (fun x -> x)");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> fun z -> let b = x y z in if b then z y else z x");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> fun z -> if x y then z x else y z");
      TestCase (fun _ -> assert_typefail "fun x -> if x then 1 else x");
      TestCase (fun _ -> assert_typefail "(fun x -> x + 1) true");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> y (x (y x))");
      TestCase (fun _ -> assert_typefail "(fun f -> fun x -> f (f x)) (fun x -> fun y -> x)");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> y (x (fun z1 -> fun z2 -> z1)) (x (fun z -> z))");
      TestCase (fun _ -> assert_typefail "fun b -> fun f -> let g1 = fun x -> f x in let g2 = fun x -> f x in if b then g1 g2 else g2 g1");
    ];
    "testcases_1" >::: [
      TestCase (fun _ -> assert_typefail "let rec f = fun x -> f in f");
      TestCase (fun _ -> assert_type "'a -> 'b" "let rec f = fun x -> f x in f");
      TestCase (fun _ -> assert_type "'a -> 'a" "let rec f = fun x -> f (f x) in f");
      TestCase (fun _ -> assert_type "(('a -> 'b) -> 'a -> 'b) -> 'a -> 'b" "let rec fix_fun = fun g -> fun x -> g (fix_fun g) x in fix_fun");
      TestCase (fun _ -> assert_type "('a -> 'a) -> 'a" "fun f -> let rec x = fun z -> f (x z) in x 666");
      TestCase (fun _ -> assert_type "int -> 'a -> 'a" "let rec f = fun x -> fun y -> if x < 0 then y else f (x + -1) y in f");
      TestCase (fun _ -> assert_type "('a -> 'b) -> ('b -> 'a) -> 'a -> 'c" "fun f -> fun g -> let rec h = fun x -> h (g (f x)) in h");
      TestCase (fun _ -> assert_type "('a -> 'a) -> 'a -> 'b" "let rec loop = fun f -> fun x -> (loop f) (f x) in loop");
      TestCase (fun _ -> assert_typefail "let rec looq = fun f -> fun x -> (looq f) (x f) in looq");
      TestCase (fun _ -> assert_type "int -> int" "fun x -> let rec f = fun y -> x + 1 in x");
      TestCase (fun _ -> assert_type "'a -> ('a -> 'a) -> int -> 'a" "let rec ind = fun x -> fun f -> fun n -> if n < 1 then x else f (ind x f (n + -1)) in ind");
      TestCase (fun _ -> assert_typefail "let rec f = fun x -> f (x f) in f");
      TestCase (fun _ -> assert_typefail "let rec f = fun z -> f z (fun g -> fun h -> h (g h)) in f");
    ];
    "testcases_2" >::: [
      TestCase (fun _ -> assert_type "'a list" "[]");
      TestCase (fun _ -> assert_type "'a list list" "[] :: []");
      TestCase (fun _ -> assert_type "'a -> 'a list -> 'a list" "fun x -> fun y -> x :: y");
      TestCase (fun _ -> assert_type "'a list -> ('a list -> 'a) -> 'a" "fun x -> fun f -> f (f x :: [])");
      TestCase (fun _ -> assert_type "int list -> int" "fun x -> match x with [] -> 0 | h :: t -> h");
      TestCase (fun _ -> assert_typefail "3 :: true :: []");
      TestCase (fun _ -> assert_typefail "fun x -> x :: x");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> x :: x y");
      TestCase (fun _ -> assert_typefail "fun x -> fun y -> fun z -> x y :: z x :: y z :: []");
      TestCase (fun _ -> assert_typefail "fun x -> match x with [] -> 0 | h :: t -> x :: t");
    ];
  ] |> run_test_tt
