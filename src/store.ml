(* PL Stores *)
(* Isaac Dunn 17/12/2015 *)

structure Expr = Expression

(* Stores are finite partial maps from locations to values *)
type store = (loc * Expr.expr) list

let string_of_store s = 
    let rec work_through left seen = match left with
        [] -> ""
      | (l, e)::es -> (if List.mem l seen then "" else (l ^ ": "))
        ^ (Expr.string_of_expr e) ^ "; "
        ^ (work_through es (l::seen))
    in work_through s []

(* get : store -> loc -> expr *)
(* get s l looks up the value in location l in store s *)
let rec get s l = match s with
    [] -> None
  | (o, e)::rest -> if o=l then Some e else get rest l

(* update : store -> loc -> expr -> store *)
(* Gives store with given location updated to new value *)
let update s l v = (l, v)::s

(* get_fresh_loc : store -> loc *)
(* Gives a location unused in the given store *)
let get_fresh_loc s = let fl = ref "L0" in while (get s !fl != None) do
                        fl := !fl ^ string_of_int(Random.int 10) done; !fl

