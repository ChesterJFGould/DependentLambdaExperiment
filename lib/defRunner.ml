open DataTypes
open ModuleTypes

module Make (Env : sig
	type t

	val add : ident -> cterm -> t -> t
end) (C : Checker with type env = Env.t) = struct
	type env = Env.t

	let run_def env =
		let (let*) = Result.bind
	in function
	| DLet (var, maybe_type, val_) ->
		let* c_val = match maybe_type with
			| None -> C.check env val_
			| Some type_ ->
				let* c_type = C.check env type_
				in C.check_of_type env val_ c_type
		in Ok (Env.add (gen_ident var) c_val env)

	let rec run_defs env =
		let (let*) = Result.bind
	in function
	| [] -> Ok env
	| def :: rest ->
		let* new_env = run_def env def
		in run_defs new_env rest
end
