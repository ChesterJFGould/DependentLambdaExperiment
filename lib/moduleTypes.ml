open DataTypes
open Format

module type Normalizer = sig
	type env

	val normalize : env -> cterm -> cterm
end

module type Typeof = sig
	type env

	val typeof : env -> cterm -> cterm
end

module type Uniquify = sig
	type env

	val uniquify : env -> term -> (uterm, string) result
end

module type TypeChecker = sig
	type env

	val type_check : env -> uterm -> (cterm, string) result
	val check_of_type : env -> uterm -> cterm -> (cterm, string) result
end

module type PrettyPrinter = sig
	val fprint_uterm : formatter -> uterm -> unit
	val fprint_cterm : formatter -> cterm -> unit
end

module type AlphaEquivChecker = sig
	val alpha_equiv : cterm -> cterm -> bool
end

module type Evaluator = sig
	include Normalizer

	type value

	val evaluate : env -> cterm -> value
end

module type DefRunner = sig
	type env

	val run_def : env -> def -> (env, string) result
	val run_defs : env -> def list -> (env, string) result
end

module type Checker = sig
	type env

	val check : env -> term -> (cterm, string) result

	val check_of_type : env -> term -> cterm -> (cterm, string) result
end

module type Executor = sig
	type env

	type prog

	val check : env -> term -> (prog, string) result

	val exec : env -> prog -> unit
end
