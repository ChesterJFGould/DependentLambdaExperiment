open DataTypes

module Make (Env : sig
	type t

	val add_uniq : string -> ident -> t -> t
	val find_uniq : string -> t -> ident option
end) : sig
	type env = Env.t

	val uniquify : env -> term -> (uterm, string) result
end
