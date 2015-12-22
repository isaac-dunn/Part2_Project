type 'a varray

exception Invalid_argument

val get : 'a varray -> int -> 'a

val set : 'a varray -> int -> 'a -> unit

val empty : unit -> 'a varray

val append : 'a varray -> 'a -> unit

val remove_last : 'a varray -> unit
