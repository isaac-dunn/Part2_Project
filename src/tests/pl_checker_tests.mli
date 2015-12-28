(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module PLCorrectnessTest :
    functor(Chk : Interfaces.Checker with module ProgImp = Program.PLProgram) -> Interfaces.Test

module SPLCheckerCorrectnessTest : Interfaces.Test

module DPORPLCheckerCorrectnessTest : Interfaces.Test

