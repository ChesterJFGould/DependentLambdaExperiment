open ModuleTypes

module Make (Env : sig type t end) (U : Uniquify with type env = Env.t) (TC : TypeChecker with type env = Env.t) = struct
	type env = Env.t

	let check env t =
		let (let*) = Result.bind
		in let* u_t = U.uniquify env t
		in TC.type_check env u_t

	let check_of_type env t t_t =
		let (let*) = Result.bind
		in let* u_t = U.uniquify env t
		in TC.check_of_type env u_t t_t
end
