(* PL Stores *)
(* Isaac Dunn 17/12/2015 *)

module Expr = Expression

(* Stores are finite partial maps from locations to values *)
type store = (Expr.loc * Expr.expr) list

type store_update = Expr.loc * Expr.expr

let string_of_store s = 
    let rec work_through left seen = match left with
        [] -> ""
      | (l, e)::es -> (if List.mem l seen then "" else (l ^ ": "))
        ^ (Expr.string_of_expr e) ^ "; "
        ^ (work_through es (l::seen))
    in work_through s []

let empty = []

(* get : store -> loc -> expr *)
(* get s l looks up the value in location l in store s *)
let rec get s l = match s with
    [] -> None
  | (o, e)::rest -> if o=l then Some e else get rest l

(* update : store -> store_update -> store *)
(* Gives store with given location updated to new value *)
let update s su = su::s

(* extend : store -> store -> store *)
(* Updates store with updates given in 2nd arg *)
let extend old_s new_s  = new_s @ old_s

(* get_fresh_loc : store -> loc *)
(* Gives a location unused in the given store *)
let get_fresh_loc s = let fl = ref "L0" in while (get s !fl != None) do
                        fl := !fl ^ string_of_int(Random.int 10) done; !fl

