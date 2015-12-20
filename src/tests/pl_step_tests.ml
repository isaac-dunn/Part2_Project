(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module Thr : (Interfaces.Thread with type ExpImp.expr = Pl_expression.expr) = Pl_thread

open Pl_expression
open Type

(* (Expression, Expected result) pairs *)
let test_cases = [
    (* Integer operations *)
    (Op (Integer 3, Plus, Integer 7), Integer 10);
    (Op (Integer 3, Minus, Integer 7), Integer (-4));
    (Op (Integer 3, Mult, Integer 7), Integer 21);
    (Op (Integer 3, Div, Integer 7), Integer 0);
    (Op (Integer 3, Mod, Integer 7), Integer 3);
    (Op (Integer 3, GT, Integer 7), Boolean false);
    (Op (Integer 3, Equals, Integer 7), Boolean false);

    (* If then else *)
    (If (Boolean true, Integer 1, Integer 2), Integer 1);
    (If (Op (Integer 4, Equals, Integer 3), Integer 1, Integer 2), Integer 2);
    (If (Skip, Integer 3, Integer 3), If (Skip, Integer 3, Integer 3));

    (* Sequences *)
    (Seq (Skip, Integer 4), Integer 4);

    (* Let *)
    (convert_from_raw (Let_raw ("x", Int, Integer_raw 9, Var_raw "x")), Integer 9);
    (convert_from_raw (Let_raw ("w", Int, Integer_raw 10,
                    Let_raw ("u", Int, Integer_raw 8, Op_raw (Var_raw "w", Minus, Var_raw "u")))),
        Integer 2);

    (* References *)
    (Deref (Ref (Integer 9)), Integer 9);
    (convert_from_raw (Let_raw ("x", Ref Int, Ref_raw (Integer_raw 6),
                    Seq_raw (Assign_raw (Var_raw "x", Op_raw (Deref_raw (Var_raw "x"), Plus, Integer_raw 7)),
                         Deref_raw (Var_raw "x")))), Integer 13);

    (* Functions *)
    (convert_from_raw (App_raw (Fn_raw ("x", Int, Op_raw (Var_raw "x", Mult, Integer_raw 3)), Integer_raw 9)), Integer 27);
    (convert_from_raw (Let_raw ("f", Func(Int, Int), Fn_raw ("x", Int, Op_raw (Var_raw "x", Plus, Integer_raw 1)),
                    App_raw (Var_raw "f", Integer_raw 7))), Integer 8);


    (* While *)
    (While (Boolean false, Integer 7), Skip);
    (convert_from_raw (Let_raw ("x", Ref Int, Ref_raw (Integer_raw 34),
                    Seq_raw (While_raw (Op_raw (Deref_raw (Var_raw "x"), GT, Integer_raw 10),
                        Assign_raw (Var_raw "x", Op_raw (Deref_raw (Var_raw "x"), Minus, Integer_raw 10))), Deref_raw (Var_raw "x")))),
        Integer 4); (* subtract 10 from 34 until at most 10 *)

    (* Let rec *)
    (convert_from_raw (Letrec_raw ("f", Int, "x", Int, Var_raw "x", App_raw (Var_raw "f", Integer_raw 5))), Integer 5);
    (convert_from_raw (Letrec_raw ("f", Int, "x", Int,
                    If_raw (Op_raw (Var_raw "x", GT, Integer_raw 32),
                        App_raw (Var_raw "f", Op_raw (Var_raw "x", Div, Integer_raw 2)),
                        Var_raw "x"),
                    App_raw (Var_raw "f", Integer_raw 100))),
        Integer 25);
    (convert_from_raw (Letrec_raw ("f", Int, "x", Int,
                    If_raw (Op_raw (Var_raw "x", Equals, Integer_raw 0), Integer_raw 1,
                            Op_raw (Var_raw "x", Mult, App_raw (Var_raw "f", Op_raw (Var_raw "x", Minus, Integer_raw 1)))),
                    App_raw (Var_raw "f", Integer_raw 4))),
        Integer 24)
]

let run_test (e, expected) =
    let unpack su s = match su with None -> s
                                  | Some x -> Thr.StoreImp.update s x in
    let rec eval (e, s, g) = match Thr.next_step (e, s, g) with
        None -> e
      | Some nsx -> eval (nsx.new_expr, unpack nsx.s_update s, unpack nsx.g_update g)
    in if eval (e, Thr.StoreImp.empty, Thr.StoreImp.empty) = expected
        then true (* success *)
        else (print_string "Expected: ";
             print_string (Thr.ExpImp.string_of_expr expected);
             print_newline ();
             print_string "Actual: ";
             print_string (Thr.ExpImp.string_of_expr (eval (e, Thr.StoreImp.empty, Thr.StoreImp.empty)));
             false) (* fail *)

let all_tests_passed =
    let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
        conj (List.map run_test test_cases)

let () = print_string ("PL Thread Next Step Tests All Passed: "
                       ^ (string_of_bool all_tests_passed));
         print_newline ()

