open DataTypes
open ModuleTypes

type 'v env_t

module type T = sig
	type t

	type value

	val empty : t
	val add : ident -> cterm -> t -> t

	val add_eval : ident -> value -> t -> t
	val find_eval : ident -> t -> value option

	val add_read : ident -> t -> t
	val mem_read : ident -> t -> bool

	val add_type : ident -> cterm -> t -> t
	val find_type : ident -> t -> cterm option

	val add_uniq : string -> ident -> t -> t
	val find_uniq : string -> t -> ident option
end

module Make (E : sig
	type value

	type env = value env_t

	val evaluate : env -> cterm -> value
end) (T : Typeof with type env = E.env) : sig
	include T with type value = E.value and type t = E.value env_t
end
