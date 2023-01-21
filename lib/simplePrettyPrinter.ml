open DataTypes
open Format

let rec a_fprint fprint_v fprint_t fprint_b ppf (t : ('v, 't, 'b) a_term) =
	let fprint = a_fprint fprint_v fprint_t fprint_b
	in let fprint_arg ppf = function
	| Lam _ | App _ | If _ | FloatOp _ | PairT _ | Let _ as t -> fprintf ppf "@[<2>(%a)@]" fprint t
	| t -> fprint ppf t
	in let rec fprints ppf = function
	| [] -> fprintf ppf ""
	| (t :: rest_t) -> fprintf ppf "%a, %a" fprint t fprints rest_t
	in let float_op_as_string = function
	| Num Add -> "+"
	| Num Sub -> "-"
	| Num Mul -> "*"
	| Num Div -> "/"
	| Cmp Eq -> "="
	| Cmp Neq -> "/="
	| Cmp Lt -> "<"
	| Cmp Lte -> "<="
	| Cmp Gt -> ">"
	| Cmp Gte -> ">="
	and action_as_string = function
	| PrintFloat -> "printFloat"
in match t with
| Float f -> fprintf ppf "%g" f
| FloatT -> fprintf ppf "Float"
| FloatOp (op, l, r) -> fprintf ppf "%a@[<2>@ %s %a@]" fprint l (float_op_as_string op) fprint r
| Bool b -> fprintf ppf "%B" b
| BoolT -> fprintf ppf "Bool"
| Unit -> fprintf ppf "()"
| UnitT -> fprintf ppf "Unit"
| Var v -> fprint_v ppf v
| Lam (v, v_t, b) -> fprintf ppf "@[<2>λ%a%a.@,%a@]" fprint_b v fprint_t v_t fprint b
| Pi (v, v_t, b) -> fprintf ppf "@[<2>∀%a:%a.@,%a@]" fprint_b v fprint v_t fprint b
| App (Lam _ as f, a) -> fprintf ppf "@[<2>(%a)@ %a@]" fprint f fprint_arg a
| App (f, a) -> fprintf ppf "@[<2>%a@ %a@]" fprint f fprint_arg a
| If (p, c, a) -> fprintf ppf "if %a@[@ then %a@ else %a@]" fprint p fprint c fprint a
| Ann (t, t_t) -> fprintf ppf "@[<2>%a@ : %a@]" fprint t fprint t_t
| Pair (l, r) -> fprintf ppf "@[<2>(%a,@ %a)@]" fprint l fprint r
| PairT (l, r) -> fprintf ppf "@[<2>(%a ^@ %a)@]" fprint l fprint r
| Left t -> fprintf ppf "(%a).l" fprint t
| Right t -> fprintf ppf "(%a).r" fprint t
| Action (action, rw, args) -> fprintf ppf "@[<2>{%s,@ %a,@ %a}@]" (action_as_string action) fprint rw fprints args
| RealWorld _ -> assert false
| RealWorldT -> fprintf ppf "RealWorld"
| Let (var, val_, body) -> fprintf ppf "@[<2>let %a =@ %a@ in %a" fprint_b var fprint val_ fprint body
| Letrec (var, var_t, val_, body) -> fprintf ppf "@[<2>letrec %a@ : %a =@ %a@ in %a" fprint_b var fprint var_t fprint val_ fprint body
| Star -> fprintf ppf "Type"
| Box -> fprintf ppf "□"

let rec fprint_cterm ppf t =
	a_fprint
		Ident.(fun ppf (v, _) -> fprintf ppf "%s" v.name)
		(fun ppf t -> fprintf ppf ":%a" fprint_cterm t)
		Ident.(fun ppf v -> fprintf ppf "%s" v.name)
		ppf
		t

let rec fprint_uterm ppf t =
	a_fprint
		Ident.(fun ppf v -> fprintf ppf "%s" v.name)
		(fun ppf -> function | None -> fprintf ppf "" | Some t -> fprintf ppf ":%a" fprint_uterm t)
		Ident.(fun ppf v -> fprintf ppf "%s" v.name)
		ppf
		t
