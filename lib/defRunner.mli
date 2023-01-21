open DataTypes
open ModuleTypes

module Make (Env : sig
	type t

	val add : ident -> cterm -> t -> t
end) (C : Checker with type env = Env.t) : sig
	type env = Env.t

	val run_def : env -> def -> (env, string) result

	val run_defs : env -> def list -> (env, string) result
end
