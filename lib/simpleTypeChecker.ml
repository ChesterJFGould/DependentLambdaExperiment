open DataTypes
open ModuleTypes
open Format

module Make (Env : sig
	type t

	val add_type : ident -> cterm -> t -> t
	val find_type : ident -> t -> cterm option
end) (N : Normalizer with type env = Env.t) (PP : PrettyPrinter) (Eq : AlphaEquivChecker) = struct
	open PP
	open Eq

	type env = Env.t

	let rec check_type env typ =
		let (let*) = Result.bind
	in function
	| Lam (v, None, b) as t -> begin match typ with
		| Pi (_, v_t, b_t) ->
			let* c_b = check_type (Env.add_type v v_t env) b_t b
			in Ok (Lam (v, v_t, c_b))
		| _ -> Error (asprintf "@[Expected@ `%a` to have a function type, instead has type@ `%a`@]" fprint_uterm t fprint_cterm typ)
		end
	| If (p, c, a) ->
		let* c_p = check_type env BoolT p
		in let* c_c = check_type env typ c
		in let* c_a = check_type env typ a
		in Ok (If (c_p, c_c, c_a))
	| t ->
		let* (c_t, t_t) = synth_type env t
		in if alpha_equiv typ t_t
		then Ok c_t
		else Error (asprintf "@[Expected@ `%a` to have type@ `%a`, instead is of type@ `%a`@]" fprint_uterm t fprint_cterm typ fprint_cterm t_t)
	and synth_type env : uterm -> (cterm * cterm, string) result =
		let (let*) = Result.bind
	in function
	| Float n -> Ok (Float n, FloatT)
	| FloatT -> Ok (FloatT, Star)
	| FloatOp (Num _ as op, l, r) ->
		let* c_l = check_type env FloatT l
		in let* c_r = check_type env FloatT r
		in Ok (FloatOp (op, c_l, c_r), FloatT)
	| FloatOp (Cmp _ as op, l, r) ->
		let* c_l = check_type env FloatT l
		in let* c_r = check_type env FloatT r
		in Ok (FloatOp (op, c_l, c_r), BoolT)
	| Bool b -> Ok (Bool b, BoolT)
	| BoolT -> Ok (BoolT, Star)
	| Unit -> Ok (Unit, UnitT)
	| UnitT -> Ok (UnitT, Star)
	| Var v -> begin match Env.find_type v env with
		| None -> Error (sprintf "(synth_type) Undefined variable `%s`" v.name)
		| Some v_t -> Ok (Var (v, v_t), v_t)
		end
	| Lam (_, None, _) as t -> Error (asprintf "@[Term@ `%a` must be given a type annotation@]" fprint_uterm t)
	| Lam (v, Some v_t, b) ->
		let* (c_v_t, _) = synth_type_type env v_t
		in let* (c_b, b_t) = synth_type (Env.add_type v c_v_t env) b
		in Ok (Lam (v, c_v_t, c_b), Pi (v, c_v_t, b_t))
	| Pi (v, v_t, b) ->
		let* (c_v_t, _) = synth_type_type env v_t
		in let* (c_b, b_t) = synth_type_type (Env.add_type v c_v_t env) b
		in Ok (Pi (v, c_v_t, c_b), b_t)
	| App (f, a) ->
		let* (c_f, f_t) = synth_type env f
		in begin match f_t with
		| Pi (v, v_t, b) ->
			let* c_a = check_type env v_t a
			in Ok (App(c_f, c_a), N.normalize env (App(Lam(v, v_t, b), c_a)))
		| _ -> Error (asprintf "@[`%a` was applied to non-function `%a` of type `%a`@]" fprint_uterm a fprint_uterm f fprint_cterm f_t)
		end
	| If (p, c, a) ->
		let* c_p = check_type env BoolT p
		in let* (c_c, c_t) = synth_type env c
		in let* (c_a, a_t) = synth_type env a
		in if alpha_equiv c_t a_t
		then Ok (If (c_p, c_c, c_a), c_t)
		else
			let bv = gen_ident "bv"
			in Ok (If (c_p, c_c, c_a), N.normalize env (App (Lam (bv, BoolT, If (Var (bv, BoolT), c_t, a_t)), c_p)))
	| Ann (t, t_t) ->
		let* (c_t_t, _) = synth_type_type env t_t
		in let* c_t = check_type env c_t_t t
		in Ok (Ann (c_t, c_t_t), c_t_t)
	| Pair (l, r) ->
		let* (c_l, l_t) = synth_type env l
		in let* (c_r, r_t) = synth_type env r
		in Ok (Pair (c_l, c_r), PairT (l_t, r_t))
	| PairT (l, r) ->
		let* (c_l, l_t) = synth_type_type env l
		in let* c_r = check_type env l_t r
		in Ok (PairT (c_l, c_r), l_t)
	| Left t ->
		let* (c_t, t_t) = synth_type env t
		in begin match t_t with
		| PairT (l_t, _) -> Ok (Left c_t, l_t)
		| _ -> Error (asprintf "@[Term `%a` was expected to be a tuple, instead is of type@ `%a`@]" fprint_uterm t fprint_cterm t_t)
		end
	| Right t ->
		let* (c_t, t_t) = synth_type env t
		in begin match t_t with
		| PairT (_, r_t) -> Ok (Right c_t, r_t)
		| _ -> Error (asprintf "@[Term `%a` was expected to be a tuple, instead is of type@ `%a`@]" fprint_uterm t fprint_cterm t_t)
		end
	| Action (action, rw, args) ->
		let* c_rw = check_type env RealWorldT rw
		in let* (c_args, _) = synth_types env args
		in Ok (Action (action, c_rw, c_args), PairT (action_ret_type action, RealWorldT))
	| RealWorld rw -> Ok (RealWorld rw, RealWorldT)
	| RealWorldT -> Ok (RealWorldT, Star)
	| Let (var, val_, body) ->
		let* (c_val, val_t) = synth_type env val_
		in let* (c_body, body_t) = synth_type (Env.add_type var val_t env) body
		in Ok (Let (var, c_val, c_body), body_t)
	| Letrec (var, var_t, Lam (arg, arg_t, l_body), body) ->
		let val_ = Lam (arg, arg_t, l_body)
		in let* (c_var_t, _) = synth_type_type env var_t
		in let* c_val = check_type (Env.add_type var c_var_t env) c_var_t val_
		in let* (c_body, body_t) = synth_type (Env.add_type var c_var_t env) body
		in Ok (Letrec (var, c_var_t, c_val, c_body), body_t)
	| Letrec _ as t -> Error (asprintf "@[Letrec `%a` must have lambda as body@]" fprint_uterm t)
	| Star -> Ok (Star, Box)
	| Box -> assert false
	and synth_type_type env t =
		let (let*) = Result.bind
		in let* (c_t, t_t) = synth_type env t
		in match t_t with
		| Star | Box -> Ok (N.normalize env c_t, t_t)
		| _ -> Error (asprintf "@[Invalid type@ `%a`@]" fprint_cterm t_t)
	and synth_types env =
		let (let*) = Result.bind
	in function
	| [] -> Ok ([], [])
	| (t :: rest_t) ->
		let* (c_t, t_t) = synth_type env t
		in let* (c_rest_t, rest_t_t) = synth_types env rest_t
		in Ok (c_t :: c_rest_t, t_t :: rest_t_t)

	let type_check env t =
		let (let*) = Result.bind
	in let* (c_t, _) = synth_type env t
	in Ok c_t

	let check_of_type env t t_t = check_type env t_t t
end
