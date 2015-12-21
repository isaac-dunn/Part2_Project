(* Unit tests for next transition function *)

module Thr = Pl_thread

let print_debug = true

open Pl_expression
open Type

(* ((e string, s, g), next_transition) pairs *)
let test_cases = [
   
    (("if true then !Gx else skip", [], [("x", Integer 2)]),
    Some {Thr.next_expr = Integer 2;
          Thr.s_updates = []; Thr.g_updates = [];
          Thr.g_loc = "x"});

    (("ref 0 := skip; ref 0 := skip; ref 0 := skip; cas(Gx, skip, skip); ref 0 := skip", [], [("x", Skip)]),
    Some {Thr.next_expr = Seq (Boolean true, Assign (Ref (Integer 0), Skip));
          Thr.s_updates = List.rev [("a", Skip); ("b", Skip); ("c", Skip)];
          Thr.g_updates = [("x", Skip)];
          Thr.g_loc = "x"});
]

let run_test ((e_string, s, g), exp_next_to) =
    (* True iff stores map some locations to same values *)
    (* Necessary as fresh locations are unpredictable *)
    let rec store_vals_match s1 s2 = match s1 with
        [] -> s2 = []
      | (_, v1)::rest -> match s2 with
            [] -> false
          | (_, v2)::rest2 -> v1 = v2 && store_vals_match rest rest2 in
    (* Transitions match *)
    let trans_match t1 t2 =
        t1.Thr.next_expr = t2.Thr.next_expr &&
        t1.Thr.g_updates = t2.Thr.g_updates &&
        t1.Thr.g_loc = t2.Thr.g_loc &&
        store_vals_match t1.Thr.s_updates t2.Thr.s_updates in
    let e = Pl_parser.expr_of_string e_string in
    match Thr.next_transition (e, s, g) with
      None -> if exp_next_to = None
                then (print_endline "Nones"; true)
                else (print_endline "Expected some but got none"; false)
    | Some actual_next_t -> match exp_next_to with
        None -> (print_endline "Expected none but got some"; false)
      | Some exp_next_t ->
     if trans_match actual_next_t exp_next_t then (print_string "match\n"; true) else
    ((if print_debug then
        print_string "Expected next transition of:\n";
        print_string (Thr.string_of_transition exp_next_t);
        print_string "But actual next transition was:\n";
        print_string (Thr.string_of_transition actual_next_t)
    ); false)

let pass_list = List.map run_test test_cases

let all_tests_passed =
    let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
        conj pass_list

let () = print_string ("PL Thread Next Transition Tests All Passed: "
                        ^ (string_of_bool all_tests_passed));
         print_newline ()

let () = let p b = print_string ((string_of_bool b) ^ " ") in
    let _ = List.map p pass_list in print_newline ()
