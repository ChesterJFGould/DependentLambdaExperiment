open DataTypes

let alpha_equiv a b =
	let rec alpha_equiv env a b = match (a, b) with
	| (Float a, Float b) -> a = b
	| (FloatT, FloatT) -> true
	| (FloatOp (a_op, a_l, a_r), FloatOp (b_op, b_l, b_r)) ->
		a_op = b_op && alpha_equiv env a_l b_l && alpha_equiv env a_r b_r
	| (Bool a_b, Bool b_b) -> a_b == b_b
	| (BoolT, BoolT) -> true
	| (Unit, Unit) -> true
	| (UnitT, UnitT) -> true
	| (Var (a_v, a_v_t), Var (b_v, b_v_t)) -> begin match List.assoc_opt b_v env with
		| None -> a_v = b_v
		| Some v -> a_v = v && alpha_equiv env a_v_t b_v_t
		end
	| (Lam (a_v, a_v_t, a_b), Lam (b_v, b_v_t, b_b)) ->
		alpha_equiv env a_v_t b_v_t && alpha_equiv ((b_v, a_v) :: env) a_b b_b
	| (Pi (a_v, a_v_t, a_b), Pi (b_v, b_v_t, b_b)) ->
		alpha_equiv env a_v_t b_v_t && alpha_equiv ((b_v, a_v) :: env) a_b b_b
	| (App (a_f, a_a), App (b_f, b_a)) -> alpha_equiv env a_f b_f && alpha_equiv env a_a b_a
	| (If (a_p, a_c, a_a), If (b_p, b_c, b_a)) ->
		alpha_equiv env a_p b_p && alpha_equiv env a_c b_c && alpha_equiv env a_a b_a
	| (Ann (a, _), b) -> alpha_equiv env a b
	| (a, Ann (b, _)) -> alpha_equiv env a b
	| (Pair (a_l, a_r), Pair (b_l, b_r)) -> alpha_equiv env a_l b_l && alpha_equiv env a_r b_r
	| (PairT (a_l, a_r), PairT (b_l, b_r)) -> alpha_equiv env a_l b_l && alpha_equiv env a_r b_r
	| (Left a, Left b) -> alpha_equiv env a b
	| (Right a, Right b) -> alpha_equiv env a b
	| (RealWorldT, RealWorldT) -> true
	| (Star, Star) -> true
	| (Box, Box) -> true
	| _ -> false
	in alpha_equiv [] a b
