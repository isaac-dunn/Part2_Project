module SimpleStatefulChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SDPORChecker :
    functor (Prog : Interfaces.Program) -> Interfaces.Checker

module SimpleStatefulPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)

module SDPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
