open DataTypes
open Format

module Make (Env : sig
	type t

	val add_uniq : string -> ident -> t -> t
	val find_uniq : string -> t -> ident option
end) = struct
	type env = Env.t

	let rec uniquify env =
		let (let*) = Result.bind
		in let uniquify_abs c env v v_t b =
			let u_v = gen_ident v
			in let* u_v_t = uniquify env v_t
			in let* u_b = uniquify (Env.add_uniq v u_v env) b
			in Ok (c u_v u_v_t u_b)
	in function
	| Float n -> Ok (Float n)
	| FloatT -> Ok FloatT
	| FloatOp (op, l, r) ->
		let* u_l = uniquify env l
		in let* u_r = uniquify env r
		in Ok (FloatOp(op, u_l, u_r))
	| Bool b -> Ok (Bool b)
	| BoolT -> Ok BoolT
	| Unit -> Ok Unit
	| UnitT -> Ok UnitT
	| Var v -> begin match Env.find_uniq v env with
		| None -> Error (sprintf "(uniquify) Undefined variable `%s`" v)
		| Some u_v -> Ok (Var u_v)
		end
	| Lam (v, None, b) ->
		let u_v = gen_ident v
		in let* u_b = uniquify (Env.add_uniq v u_v env) b
		in Ok (Lam (u_v, None, u_b))
	| Lam (v, Some v_t, b) -> uniquify_abs (fun v v_t b -> Lam (v, Some v_t, b)) env v v_t b
	| Pi (v, v_t, b) -> uniquify_abs (fun v v_t b -> Pi (v, v_t, b)) env v v_t b
	| App (f, a) ->
		let* u_f = uniquify env f
		in let* u_a = uniquify env a
		in Ok (App (u_f, u_a))
	| If (p, c, a) ->
		let* u_p = uniquify env p
		in let* u_c = uniquify env c
		in let* u_a = uniquify env a
		in Ok (If (u_p, u_c, u_a))
	| Ann (t, t_t) ->
		let* u_t = uniquify env t
		in let* u_t_t = uniquify env t_t
		in Ok (Ann (u_t, u_t_t))
	| Pair (l, r) ->
		let* u_l = uniquify env l
		in let* u_r = uniquify env r
		in Ok (Pair (u_l, u_r))
	| PairT (l, r) ->
		let* u_l = uniquify env l
		in let* u_r = uniquify env r
		in Ok (PairT (u_l, u_r))
	| Left t ->
		let* u_t = uniquify env t
		in Ok (Left u_t)
	| Right t ->
		let* u_t = uniquify env t
		in Ok (Right u_t)
	| Action (action, rw, args) ->
		let* u_rw = uniquify env rw
		in let* u_args = uniquifys env args
		in Ok (Action (action, u_rw, u_args))
	| RealWorld rw -> Ok (RealWorld rw)
	| RealWorldT -> Ok RealWorldT
	| Let (var, val_, body) ->
		let u_var = gen_ident var
		in let* u_val = uniquify env val_
		in let* u_body = uniquify (Env.add_uniq var u_var env) body
		in Ok (Let (u_var, u_val, u_body))
	| Letrec (var, var_t, val_, body) ->
		let* u_var_t = uniquify env var_t
		in let u_var = gen_ident var
		in let* u_val = uniquify (Env.add_uniq var u_var env) val_
		in let* u_body = uniquify (Env.add_uniq var u_var env) body
		in Ok (Letrec (u_var, u_var_t, u_val, u_body))
	| Star -> Ok Star
	| Box -> Ok Box
	and uniquifys env =
		let (let*) = Result.bind
	in function
	| [] -> Ok []
	| (t :: rest_t) ->
		let* u_t = uniquify env t
		in let* u_rest_t = uniquifys env rest_t
		in Ok (u_t :: u_rest_t)
end
