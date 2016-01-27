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
    ("23 = 7", "false");

    (* If then else *)
    ("if true then 1 else 2", "1");
    ("if 4 = 3 then 1 else 2", "2");

    (* Sequences *)
    ("skip; 4", "4");

    (* Let *)
    ("let Vx = 9 in Vx", "9");
    ("let Vw = 10 in let Vu = 8 in Vw - Vu", "2");

    (* Skip *)
    ("skip", "skip");

    (* References *)
    ("!(ref 9)", "9");
    ("(ref 6) := 4", "skip");
    ("let Vx = ref 6 in Vx := !Vx + 7; !Vx", "13");

    (* Functions *)
    ("(fn Vx => Vx * 3) @ 9", "27");
    ("let Vf = fn Vx => 1 + Vx in Vf @ 7", "8");

    (* While *)
    ("while false do skip done", "skip");
    ("let Vx = ref false in while false do Vx := true done; !Vx", "false");
    ("let Vx = ref 34 in while !Vx > 10 do Vx := !Vx - 10 done; !Vx", "4");

    (* Let rec *)
    ("let rec Vf = fn Vx => Vx - 1 in Vf @ 5", "4");
    ("let rec Vg = fn Vy => if 32 > Vy then Vy else Vg @ (Vy/2) in Vg @ 100", "25");
    ("let rec Vfact = fn Vz => if Vz = 0 then 1 else Vz * (Vfact @ (Vz-1)) in Vfact @ 5", "120");

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

