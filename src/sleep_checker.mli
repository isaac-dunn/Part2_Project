module SimpleSleepChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module DPORSleepChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SimplePLSleepChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module DPORPLSleepChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
