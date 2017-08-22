val expr_raw_of_string : string -> Pl_expression.expr_raw

val expr_of_string : string -> Pl_expression.expr

val program_of_string : string -> Program.PLProgram.state

val program_of_channel : in_channel -> Program.PLProgram.state
