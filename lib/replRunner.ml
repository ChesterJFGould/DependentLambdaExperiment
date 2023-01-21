open DataTypes
open ModuleTypes
open Format

module Make
	(Env : sig
		type t

		val add : ident -> cterm -> t -> t
	end)
	(C : Checker with type env = Env.t)
	(DR : DefRunner with type env = Env.t)
	(PP : PrettyPrinter)
	(T : Typeof with type env = Env.t)
	(N : Normalizer with type env = Env.t)
	(E : Executor with type env = Env.t)
	= struct
	open PP

	let run_repl env =
		let (let*) = Result.bind
	in function
	| RLet (v, t) ->
		let* c_t = C.check env t
		in fprintf std_formatter "%s : %a\n" v fprint_cterm (T.typeof env c_t);
		Ok (Env.add (gen_ident v) c_t env)
	| RNorm t ->
		let* c_t = C.check env t
		in fprintf std_formatter "Done checking@.";
		let n_c_t = N.normalize env c_t
		in fprintf std_formatter "%a : %a\n" fprint_cterm n_c_t fprint_cterm (T.typeof env n_c_t);
		Ok env
	| RExec t ->
		let* c_t = E.check env t
		in E.exec env c_t;
		Ok env
	| RLoad path ->
		let ch = open_in path
		in begin match Parsers.defs (Stream.of_channel ch) with
		| None -> Error "Parse Error"
		| Some defs -> DR.run_defs env defs
		end
end
