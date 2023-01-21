open Format
open Opal

type term
type cterm
type repl
type prog

type env

val default_env : env

val run_prog : env -> prog -> unit

val run_repl : env -> repl -> (env, string) result

val parse_term : char Stream.t -> term option

val parse_repl : char Stream.t -> repl option

val check : env -> term -> (cterm, string) result

val check_prog : env -> term -> (prog, string) result

val normalize : env -> cterm -> cterm

val typeof : env -> cterm -> cterm

val fprint_cterm : formatter -> cterm -> unit
