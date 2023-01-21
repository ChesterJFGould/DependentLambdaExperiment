open DataTypes
open ModuleTypes

module Make (Env : sig
	type t

	val add_type : ident -> cterm -> t -> t
	val find_type : ident -> t -> cterm option
end) (N : Normalizer with type env = Env.t) (PP : PrettyPrinter) (Eq : AlphaEquivChecker) : sig
	type env = Env.t

	val type_check : env -> uterm -> (cterm, string) result
	val check_of_type : env -> uterm -> cterm -> (cterm, string) result
end
