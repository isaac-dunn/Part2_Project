module PLCorrectnessTest :
    functor(Chk : Interfaces.Checker with module ProgImp = Program.PLProgram) -> Interfaces.Test

module SimplePLCheckerCorrectnessTest : Interfaces.Test

module DPORPLCheckerCorrectnessTest : Interfaces.Test

module SPORPLCheckerCorrectnessTest : Interfaces.Test

module SimpleStatefulPLCheckerCorrectnessTest : Interfaces.Test

module SDPORPLCheckerCorrectnessTest : Interfaces.Test

module SimpleSleepPLCheckerCorrectnessTest : Interfaces.Test

module DPORSleepPLCheckerCorrectnessTest : Interfaces.Test

module SPORSleepPLCheckerCorrectnessTest : Interfaces.Test
