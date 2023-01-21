type float_num_op =
| Add
| Sub
| Mul
| Div

type float_cmp_op =
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte

type float_op =
| Num of float_num_op
| Cmp of float_cmp_op

module Ident = struct
	type t = { name: string; id: int }

	let compare = Stdlib.compare
end

type ident = Ident.t

type real_world = RW

type action =
| PrintFloat

type ('v, 't, 'b) a_term =
| Float of float
| Bool of bool
| FloatT
| BoolT
| Unit
| UnitT
| FloatOp of float_op * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Var of 'v
| Lam of 'b * 't * ('v, 't, 'b) a_term
| Pi of 'b * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| App of ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| If of ('v, 't, 'b) a_term * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Ann of ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Pair of ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| PairT of ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Left of ('v, 't, 'b) a_term
| Right of ('v, 't, 'b) a_term
| Action of action * ('v, 't, 'b) a_term * (('v, 't, 'b) a_term) list
| RealWorld of real_world
| RealWorldT
| Let of 'b * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Letrec of 'b * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term * ('v, 't, 'b) a_term
| Star
| Box

type term = (string, term option, string) a_term

type uterm = (Ident.t, uterm option, Ident.t) a_term

type cterm = ((Ident.t * cterm), cterm, Ident.t) a_term

type repl =
| RLet of string * term
| RNorm of term
| RExec of term
| RLoad of string

type def =
| DLet of string * term option * term

let action_ret_type = function
| PrintFloat -> UnitT

let gen_ident =
	let id = ref 0
	in fun s ->
		let i = Ident.{ name = s; id = !id }
		in id := !id + 1; i

