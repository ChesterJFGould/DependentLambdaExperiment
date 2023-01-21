open DataTypes;;
open ModuleTypes;;

type value_t

module Make (Env : sig
	type t

	val add_eval : ident -> value_t -> t -> t
	val find_eval : ident -> t -> value_t option

	val add_read : ident -> t -> t
	val mem_read : ident -> t -> bool
end) : sig
	type env = Env.t

	type value = value_t

	val normalize : env -> cterm -> cterm

	val evaluate : env -> cterm -> value
end
