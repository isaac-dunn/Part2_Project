(* Programs *)
(* Isaac Dunn 17/12/2015 *)

module Program :
    functor (Thr : Interfaces.Thread) -> Interfaces.Program

module PLProgram : (Interfaces.Program
    with module ThrImp = Pl_thread)

