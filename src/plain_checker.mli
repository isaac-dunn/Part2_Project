module SimpleChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module DPORChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SPORChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module DPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module SPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
