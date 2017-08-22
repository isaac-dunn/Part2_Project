module ListStore :
    functor (Expr : Interfaces.Expression) -> Interfaces.Store

module PLStore : (Interfaces.Store
    with type ExprImp.expr = Pl_expression.expr
    and type ExprImp.loc = Pl_expression.loc
    and type store = (Pl_expression.loc * Pl_expression.expr) list)
