open DataTypes

val repl : char Stream.t -> repl option
val term : char Stream.t -> term option
val def : char Stream.t -> def option
val defs : char Stream.t -> (def list) option
