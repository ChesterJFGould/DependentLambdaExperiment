open DataTypes
open ModuleTypes

module Make (Env : sig type t end) (U : Uniquify with type env = Env.t) (TC : TypeChecker with type env = Env.t) : sig
	type env = Env.t

	val check : env -> term -> (cterm, string) result

	val check_of_type : env -> term -> cterm -> (cterm, string) result
end
