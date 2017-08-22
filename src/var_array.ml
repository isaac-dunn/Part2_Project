(* user-aware length, array *)
type 'a varray = int ref * 'a array ref

exception Invalid_argument

let length (ul, a) = !ul

let get (ul, a) i =
    if i < !ul then Array.get !a i
      else raise Invalid_argument

let set (ul, a) i x =
    if i < !ul then Array.set !a i x
    else raise Invalid_argument

let empty () = (ref 0, ref [||])

let append (ul, a) x =
    (if !ul < Array.length !a then
        Array.set !a !ul x
     else
        (* 1 + len as 2 * 0 = 0 *)
        a := Array.append !a (Array.make (1 + Array.length !a) x));
    ul := !ul + 1 

let remove_last (ul, a) = ul := !ul - 1

