(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

#use "lang.ml";;

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
    (resolve [] (Let_raw ("x", Int, Integer_raw 9, Var_raw "x")), Integer 9);
    (resolve [] (Let_raw ("w", Int, Integer_raw 10,
                    Let_raw ("u", Int, Integer_raw 8, Op_raw (Var_raw "w", Minus, Var_raw "u")))),
        Integer 2);

    (* References *)
    (Deref (Ref (Integer 9)), Integer 9);
    (resolve [] (Let_raw ("x", Ref Int, Ref_raw (Integer_raw 6),
                    Seq_raw (Assign_raw (Var_raw "x", Op_raw (Deref_raw (Var_raw "x"), Plus, Integer_raw 7)),
                         Deref_raw (Var_raw "x")))), Integer 13);

    (* Functions *)
    (resolve [] (App_raw (Fn_raw ("x", Int, Op_raw (Var_raw "x", Mult, Integer_raw 3)), Integer_raw 9)), Integer 27);
    (resolve [] (Let_raw ("f", Func(Int, Int), Fn_raw ("x", Int, Op_raw (Var_raw "x", Plus, Integer_raw 1)),
                    App_raw (Var_raw "f", Integer_raw 7))), Integer 8);


    (* While *)
    (While (Boolean false, Integer 7), Skip);
    (resolve [] (Let_raw ("x", Ref Int, Ref_raw (Integer_raw 34),
                    Seq_raw (While_raw (Op_raw (Deref_raw (Var_raw "x"), GT, Integer_raw 10),
                        Assign_raw (Var_raw "x", Op_raw (Deref_raw (Var_raw "x"), Minus, Integer_raw 10))), Deref_raw (Var_raw "x")))),
        Integer 4); (* subtract 10 from 34 until at most 10 *)

    (* Let rec *)
    (resolve [] (Letrec_raw ("f", Int, "x", Int, Var_raw "x", App_raw (Var_raw "f", Integer_raw 5))), Integer 5);
    (*(resolve [] (Letrec_raw ("f", Int, "x", Int, App_raw (Var_raw "f", Var_raw "x"), App_raw (Var_raw "f", Integer_raw 4))),
        Integer 4);*)
    (resolve [] (Letrec_raw ("f", Int, "x", Int,
                    If_raw (Op_raw (Var_raw "x", GT, Integer_raw 32),
                        App_raw (Var_raw "f", Op_raw (Var_raw "x", Div, Integer_raw 2)),
                        Var_raw "x"),
                    App_raw (Var_raw "f", Integer_raw 100))),
        Integer 25);
    (resolve [] (Letrec_raw ("f", Int, "x", Int,
                    If_raw (Op_raw (Var_raw "x", Equals, Integer_raw 0), Integer_raw 1,
                            Op_raw (Var_raw "x", Mult, App_raw (Var_raw "f", Op_raw (Var_raw "x", Minus, Integer_raw 1)))),
                    App_raw (Var_raw "f", Integer_raw 4))),
        Integer 24)
]

let rec test_seq cases = match cases with
    [] -> print_string "All test cases passed\n"
  | ((prog, expected)::rest) -> let (e, s, g) =  evaluate (prog, [], []) in
            if e = expected then test_seq rest else print_exec(prog, [], []);;

test_seq test_cases;;

let conc_cases = [
    (* thread 1, thread 2, global store, error possible *)
    (If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS"),
     Cas(Glo "x", Integer 2, Integer 1), [("x", Integer 2)], true);

    (If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS"),
     Cas(Glo "x", Integer 2, Integer 2), [("x", Integer 2)], false);

    (If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS"),
     Cas(Glo "x", Integer 1, Integer 1), [("x", Integer 2)], false);

    (If (Cas (Glo "x", Integer 1, Integer 0), Integer 100, Error "Negative CAS"),
     Cas(Glo "x", Integer 2, Integer 1), [("x", Integer 2)], true);

    (While (Cas (Glo "x", Boolean false, Skip), Error "Unreachable"),
     While (Cas (Glo "x", Skip, Boolean true), Error "Unreachable"),
     [("x", Integer 5)], false);

    (While (Cas (Glo "x", Boolean false, Skip), Error "Unreachable"),
     While (Cas (Glo "x", Skip, Boolean true), Error "Unreachable"),
     [("x", Skip)], true);

    (If (Cas (Glo "x", Deref (Glo "x"), Integer 0), Skip, Error "Cas failed"),
     If (Cas (Glo "x", Deref (Glo "x"), Integer 1), Skip, Error "Cas failed"),
     [("x", Integer 2)], true);

    (If (Cas (Glo "x", Deref (Glo "x"), Integer 2), Skip, Error "Cas failed"),
     If (Cas (Glo "x", Deref (Glo "x"), Integer 2), Skip, Error "Cas failed"),
     [("x", Integer 2)], false);
];;

let rec test_conc cases = match cases with
    [] -> print_string "All test cases passed\n"
  | (e1, e2, g, err_reachable)::rest -> 
        let tds = Array.make 2 (Skip, []) in
        Array.set tds 0 (e1, []);
        Array.set tds 1 (e2, []);
        let (err, sanv) = check_program (tds, g) [] in
            if (err <> err_reachable) then (print_string "Unexpectd err value\n"; explore (tds, g) []) else
            if sanv then (print_string "Stuck at non-value\n"; explore (tds, g) []) else
            test_conc rest;;

test_conc conc_cases;;
