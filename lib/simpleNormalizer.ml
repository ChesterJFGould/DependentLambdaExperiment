open DataTypes
open Format

type value_t =
| VFloat of float
| VFloatT
| VBool of bool
| VBoolT
| VUnit
| VUnitT
| VLam of Ident.t * value_t * (value_t -> value_t)
| VPi of Ident.t * value_t * (value_t -> value_t)
| VPair of value_t * value_t 
| VPairT of value_t * value_t
| VRealWorld of real_world
| VRealWorldT
| VStar
| VBox
| VNeu of neutral
and neutral =
| NVar of Ident.t * value_t
| NApp of neutral * value_t
| NIf of neutral * value_t * value_t
| NFloatOpL of float_op * neutral * value_t
| NFloatOpR of float_op * value_t * neutral
| NLeft of neutral
| NRight of neutral
| NAction of action * value_t * value_t list

module Make (Env : sig
	type t

	val add_eval : ident -> value_t -> t -> t
	val find_eval : ident -> t -> value_t option

	val add_read : ident -> t -> t
	val mem_read : ident -> t -> bool
end) = struct
	type env = Env.t

	type value = value_t

	let rec evaluate (env : env) : cterm -> value =
		let run_num_op op l r = match op with
		| Add -> l +. r
		| Sub -> l -. r
		| Mul -> l *. r
		| Div -> l /. r
		and run_cmp_op op l r = match op with
		| Eq -> l = r
		| Neq -> not (l = r)
		| Lt -> l < r
		| Lte -> l <= r
		| Gt -> l > r
		| Gte -> l >= r
		and run_action action rw args = match (action, args) with
		| (PrintFloat, [VFloat f]) -> printf "%g\n" f; (VUnit, rw)
		| _ -> assert false
	in function
	| Float f -> VFloat f
	| FloatT -> VFloatT
	| FloatOp (Num op, l, r) -> begin match (evaluate env l, evaluate env r) with
		| (VFloat l, VFloat r) -> VFloat (run_num_op op l r)
		| (VNeu l, r) -> VNeu (NFloatOpL (Num op, l, r))
		| (l, VNeu r) -> VNeu (NFloatOpR (Num op, l, r))
		| _ -> assert false
		end
	| FloatOp (Cmp op, l, r) -> begin match (evaluate env l, evaluate env r) with
		| (VFloat l, VFloat r) -> VBool (run_cmp_op op l r)
		| (VNeu l, r) -> VNeu (NFloatOpL (Cmp op, l, r))
		| (l, VNeu r) -> VNeu (NFloatOpR (Cmp op, l, r))
		| _ -> assert false
		end
	| Bool b -> VBool b
	| BoolT -> VBoolT
	| Unit -> VUnit
	| UnitT -> VUnitT
	| Var (v, v_t) -> begin match Env.find_eval v env with
		| None -> VNeu (NVar (v, evaluate env v_t))
		| Some v -> v
		end
	| Lam (v, v_t, b) -> VLam (v, evaluate env v_t, fun x -> evaluate (Env.add_eval v x env) b)
	| Pi (v, v_t, b) -> VPi (v, evaluate env v_t, fun x -> evaluate (Env.add_eval v x env) b)
	| App (f, a) -> begin match evaluate env f with
		| VNeu n -> VNeu (NApp (n, evaluate env a))
		| VLam (_, _, f) -> f (evaluate env a)
		| _ -> assert false
		end
	| If (p, c, a) -> begin match evaluate env p with
		| VBool true -> evaluate env c
		| VBool false -> evaluate env a
		| VNeu n -> VNeu (NIf (n, evaluate env c, evaluate env a))
		| _ -> assert false
		end
	| Ann (t, _) -> evaluate env t
	| Pair (l, r) -> VPair (evaluate env l, evaluate env r)
	| PairT (l, r) -> VPairT (evaluate env l, evaluate env r)
	| Left t -> begin match evaluate env t with
		| VPair (l, _) -> l
		| VNeu n -> VNeu (NLeft n)
		| _ -> assert false
		end
	| Right t -> begin match evaluate env t with
		| VPair (_, r) -> r
		| VNeu n -> VNeu (NRight n)
		| _ -> assert false
		end
	| Action (action, rw, args) ->
		begin match (evaluate env rw, evaluates env args) with
		| (VNeu n_rw, (_, v_args)) -> VNeu (NAction (action, VNeu n_rw, v_args))
		| (v_rw, (false, n_args)) -> VNeu (NAction (action, v_rw, n_args))
		| (VRealWorld rw, (true, v_args)) ->
			let (ret, rw) = run_action action rw v_args
			in VPair (ret, VRealWorld rw)
		| _ -> assert false
		end
	| RealWorld rw -> VRealWorld rw
	| RealWorldT -> VRealWorldT
	| Let (var, val_, body) -> evaluate (Env.add_eval var (evaluate env val_) env) body
	| Letrec (var, var_t, Lam (arg, arg_t, l_body), body) ->
		let arg_t_v = evaluate env arg_t
		in let rec fix x = evaluate (Env.add_eval arg x (Env.add_eval var (VLam (arg, arg_t_v, fix)) env)) l_body
		in evaluate (Env.add_eval var (VLam (arg, evaluate env arg_t, fix)) env) body
	(* (VFix \fac : Float -> Float. \n : Float. if gt n 0 then mul n (fac (sub n 1)) else 1) => \n : Float. if gt n 0 then mul n ((VFix (\fac : Float -> Float. ...)) (sub n 1)) else 1 *)
	| Letrec _ -> assert false
	| Star -> VStar
	| Box -> VBox
	and evaluates env = function
	| [] -> (true, [])
	| (t :: rest_t) -> begin match evaluate env t with
		| VNeu n ->
			let (_, v_rest_t) = evaluates env rest_t
			in (false, VNeu n :: v_rest_t)
		| v_t ->
			let (is_val, v_rest_t) = evaluates env rest_t
			in (is_val, v_t :: v_rest_t)
		end

	let rec freshen v env =
		if Env.mem_read v env
		then freshen {name = v.name ^ "'"; id = v.id} env
		else v

	let rec read_back env = function
	| VFloat f -> Float f
	| VFloatT -> FloatT
	| VBool b -> Bool b
	| VBoolT -> BoolT
	| VUnit -> Unit
	| VUnitT -> UnitT
	| VLam (v, v_t, f) ->
		let v' = freshen v env
		in Lam (v', read_back env v_t, read_back (Env.add_read v' env) (f (VNeu (NVar (v', v_t)))))
	| VPi (v, v_t, f) ->
		let v' = freshen v env
		in Pi (v', read_back env v_t, read_back (Env.add_read v' env) (f (VNeu (NVar (v', v_t)))))
	| VPair (l, r) -> Pair (read_back env l, read_back env r)
	| VPairT (l, r) -> PairT (read_back env l, read_back env r)
	| VRealWorld rw -> RealWorld rw
	| VRealWorldT -> RealWorldT
	| VStar -> Star
	| VBox -> Box
	| VNeu (NVar (v, v_t)) -> Var (v, read_back env v_t)
	| VNeu (NApp (f, a)) -> App (read_back env (VNeu f), read_back env a)
	| VNeu (NIf (p, c, a)) -> If (read_back env (VNeu p), read_back env c, read_back env a)
	| VNeu (NFloatOpL (op, l, r)) -> FloatOp (op, read_back env (VNeu l), read_back env r)
	| VNeu (NFloatOpR (op, l, r)) -> FloatOp (op, read_back env l, read_back env (VNeu r))
	| VNeu (NLeft t) -> Left (read_back env (VNeu t))
	| VNeu (NRight t) -> Right (read_back env (VNeu t))
	| VNeu (NAction (action, rw, args)) -> Action (action, read_back env rw, List.map (read_back env) args)

	let normalize env t = read_back env (evaluate env t)
end
