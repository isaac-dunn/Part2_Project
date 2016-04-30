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

    let minimise s =
        let rec min_aux sto seen = match sto with
            [] -> []
          | (l, v)::xs -> if List.mem l seen then min_aux xs seen
                          else (l, v)::(min_aux xs (l::seen))
        in min_aux s []

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

    let rec globals_stored s =
        let union xs ys = (List.filter (fun x -> not (List.mem x ys)) xs) @ ys in
        match s with
            [] -> []
          | (l, e)::rest ->
                union (ExprImp.locations_accessed e) (globals_stored rest)

    let eq s t =
        let rec sub s t = match s with [] -> true
          | ((l, e)::rest) -> get t l = (Some e) && sub rest t in
        let ms = minimise s in
        let mt = minimise t in
        sub ms mt && sub mt ms

    let hash st =
        let rec haux s = match s with
            [] -> max_int
          | x::xs -> Hashtbl.hash x land haux xs
    in haux (minimise st)
end

module PLStore = ListStore (Pl_expression)
