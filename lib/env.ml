open DataTypes
open ModuleTypes

module StringMap = Map.Make(String)
module IdentMap = Map.Make(Ident)
module IdentSet = Set.Make(Ident)

type 'v env_t = {
	eval: 'v IdentMap.t;
	read: IdentSet.t;
	type_: cterm IdentMap.t;
	uniq: ident StringMap.t;
}

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
end) (T : Typeof with type env = E.env) = struct
	type t = E.value env_t

	type value = E.value

	let empty = {
		eval = IdentMap.empty;
		read = IdentSet.empty;
		type_ = IdentMap.empty;
		uniq = StringMap.empty;
	}

	let add ident term env = {
		eval = IdentMap.add ident (E.evaluate env term) env.eval;
		read = IdentSet.add ident env.read;
		type_ = IdentMap.add ident (T.typeof env term) env.type_;
		uniq = StringMap.add Ident.(ident.name) ident env.uniq;
	}

	let add_eval ident value env = { env with eval = IdentMap.add ident value env.eval }

	let add_read ident env = { env with read = IdentSet.add ident env.read }

	let add_type ident term env = { env with type_ = IdentMap.add ident term env.type_ }

	let add_uniq name ident env = { env with uniq = StringMap.add name ident env.uniq }

	let find_eval ident env = IdentMap.find_opt ident env.eval

	let mem_read ident env = IdentSet.mem ident env.read

	let find_type ident env = IdentMap.find_opt ident env.type_

	let find_uniq name env = StringMap.find_opt name env.uniq
end
