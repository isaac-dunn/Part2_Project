let check filename =
    let ic = open_in filename in
    let prog = Pl_parser.program_of_channel ic in
    close_in ic;
    Checker.DPORPLChecker.error_free prog

let () =
    if Array.length Sys.argv <> 2
    then print_endline "Pass exactly one argument, the filename of the program"
    else print_endline (string_of_bool (check Sys.argv.(1)))
    
