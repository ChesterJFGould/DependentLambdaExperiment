open ModuleTypes
open DataTypes

module Make
	(Env : sig type t end)
	(C : Checker with type env = Env.t)
	(N : Normalizer with type env = Env.t)
	= struct
	type env = Env.t

	type prog = cterm

	let prog_type =
		let rw = gen_ident "rw"
		in Pi (rw, RealWorldT, PairT (UnitT, RealWorldT))

	let check env t = C.check_of_type env t prog_type

	let exec env t =
		ignore (N.normalize env (App (t, RealWorld RW)));
		()
end
