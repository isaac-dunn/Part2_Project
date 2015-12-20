module SimpleChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
