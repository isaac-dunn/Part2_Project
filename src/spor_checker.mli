module SPORChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SPORSleepChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module SPORSleepPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
