open DataTypes
open ModuleTypes

module Make (Env : sig
	type t
end) (N : Normalizer with type env = Env.t) = struct
	type env = Env.t

	let rec typeof env = function
	| Float _ -> FloatT
	| FloatT -> Star
	| FloatOp(Num _, _, _) -> FloatT
	| FloatOp(Cmp _, _, _) -> BoolT
	| Bool _ -> BoolT
	| BoolT -> Star
	| Unit -> UnitT
	| UnitT -> Star
	| Var (_, t) -> t
	| Lam (v, v_t, b) -> Pi (v, v_t, typeof env b)
	| Pi (_, _, b) -> typeof env b
	| App (f, a) -> begin match typeof env f with
		| Pi (v, v_t, b) -> N.normalize env (App(Lam(v, v_t, b), a))
		| _ -> assert false
		end
	| If (_, c, _) -> typeof env c
	| Ann (_, t) -> t
	| Pair (l, r) -> PairT (typeof env l, typeof env r)
	| PairT (l, _) -> typeof env l
	| Left t -> begin match typeof env t with
		| PairT (l_t, _) -> l_t
		| _ -> assert false
		end
	| Right t -> begin match typeof env t with
		| PairT (_, r_t) -> r_t
		| _ -> assert false
		end
	| Action (action, _, _) -> PairT (action_ret_type action, RealWorldT)
	| RealWorld _ -> RealWorldT
	| RealWorldT -> Star
	| Let (_, _, body) -> typeof env body
	| Letrec (_, _, _, body) -> typeof env body
	| Star -> Box
	| Box -> assert false
end