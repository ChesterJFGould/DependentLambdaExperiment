open DataTypes
open ModuleTypes

module Make
	(Env : sig
		type t

		val add : ident -> cterm -> t -> t
	end)
	(C : Checker with type env = Env.t)
	(DR : DefRunner with type env = Env.t)
	(PP : PrettyPrinter)
	(T : Typeof with type env = Env.t)
	(N : Normalizer with type env = Env.t)
	(E : Executor with type env = Env.t)
	: sig
	val run_repl : Env.t -> repl -> (Env.t, string) result
end
