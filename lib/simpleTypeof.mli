open DataTypes
open ModuleTypes

module Make (Env : sig
	type t
end) (N : Normalizer with type env = Env.t) : sig
	type env = Env.t

	val typeof : env -> cterm -> cterm
end
