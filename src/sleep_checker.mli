module SimpleSleepChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module DPORSleepChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SimpleSleepPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module DPORSleepPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
