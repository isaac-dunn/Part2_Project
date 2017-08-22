module Program :
    functor (Thr : Interfaces.Thread) -> Interfaces.Program

module PLProgram : (Interfaces.Program
    with module ThrImp = Pl_thread)

