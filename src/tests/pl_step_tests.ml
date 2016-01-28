(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module Thr : (Interfaces.Thread with type ExpImp.expr = Pl_expression.expr) = Pl_thread

open Pl_expression
open Type

(* (Expression, Expected result) pairs *)
let test_cases = [
    (* Integer operations *)
    ("3 + 7", "10");
    ("3 - 7", "-4");
    ("3 * 7", "21");
    ("23 / 7", "3");
    ("23 % 7", "2");
    ("23 > 7", "true");
    ("23 < 7", "false");
    ("23 = 7", "false");

    (* Boolean operations *)
    ("¬true", "false");
    ("¬ false", "true");
    ("true & true", "true");
    ("true & false", "false");
    ("true | false", "true");
    ("false | false", "false");
    ("true = true", "true");
    ("true = false", "false");
    ("false = false", "true");

    (* If then else *)
    ("if true then 1 else 2", "1");
    ("if 4 = 3 then 1 else 2", "2");

    (* Sequences *)
    ("skip; 4", "4");

    (* Let *)
    ("let x = 9 in x", "9");
    ("let w = 10 in let u = 8 in w - u", "2");

    (* Skip *)
    ("skip", "skip");

    (* References *)
    ("!(ref 9)", "9");
    ("(ref 6) := 4", "skip");
    ("let x = ref 6 in x := !x + 7; !x", "13");

    (* Functions *)
    ("(fn x => x * 3) @ 9", "27");
    ("let f = fn x => 1 + x in f @ 7", "8");

    (* While *)
    ("while false do skip done", "skip");
    ("let x = ref false in while false do x := true done; !x", "false");
    ("let x = ref 34 in while !x > 10 do x := !x - 10 done; !x", "4");

    (* Let rec *)
    ("let rec f = fn x => x - 1 in f @ 5", "4");
    ("let rec g = fn y => if 32 > y then y else g @ (y/2) in g @ 100", "25");
    ("let rec fact = fn z => if z = 0 then 1 else z * (fact @ (z-1)) in fact @ 5", "120");

    (* Error *)
    ("error(oh dear)", "error(oh dear)");
]

let run_test (e_str, expected_str) =
    let e = Pl_parser.expr_of_string e_str in
    let expected = Pl_parser.expr_of_string expected_str in
    let unpack su s = match su with None -> s
                                  | Some x -> Thr.StoreImp.update s x in
    let rec eval (e, s, g) = match Thr.next_step (e, s, g) with
        None -> e
      | Some nsx -> eval (nsx.Thr.new_expr, unpack nsx.Thr.s_update s, unpack nsx.Thr.g_update g)
    in if eval (e, Thr.StoreImp.empty, Thr.StoreImp.empty) = expected
        then (print_endline ("Success: " ^ e_str ^ " --> " ^ expected_str); true) (* success *)
        else (print_string "Expected: ";
             print_endline (Thr.ExpImp.string_of_expr expected);
             print_string "Actual: ";
             print_endline (Thr.ExpImp.string_of_expr (eval (e, Thr.StoreImp.empty, Thr.StoreImp.empty)));
             false) (* fail *)

let all_tests_passed =
    let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
        conj (List.map run_test test_cases)

let () = print_string ("PL Thread Next Step Tests All Passed: "
                       ^ (string_of_bool all_tests_passed));
         print_newline ()

