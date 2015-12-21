(* Store Functors *)
(* Isaac Dunn 17/12/2015 *)

module ListStore (Expr : Interfaces.Expression) = struct
    module ExprImp = Expr

    (* Stores are finite partial maps from locations to values *)
    type store = (Expr.loc * Expr.expr) list

    type store_update = Expr.loc * Expr.expr

    let string_of_store s = 
        let rec work_through left seen = match left with
            [] -> ""
          | (l, e)::es -> (if List.mem l seen then "" else (Expr.string_of_loc l ^ ": ")
            ^ (Expr.string_of_expr e) ^ "; ")
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

    let update_if_undefined s (l, v) = match get s l with
        None -> (l, v):: s
      | Some _ -> s

    (* extend : store -> store -> store *)
    (* Updates store with updates given in 2nd arg *)
    let extend old_s new_s  = new_s @ old_s

    (* get_fresh_loc : store -> loc *)
    (* Gives a location unused in the given store *)
    let rec get_fresh_loc s =
        let rl = Expr.random_loc () in
        match get s rl with
          Some _ -> get_fresh_loc s
        | None -> rl
end

module PLStore : (Interfaces.Store
    with type ExprImp.expr = Pl_expression.expr
    and type ExprImp.loc = Pl_expression.loc
    and type store = (Pl_expression.loc * Pl_expression.expr) list)
        = ListStore (Pl_expression)
