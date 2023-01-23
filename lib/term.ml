open List
open Opal
open Format
open DataTypes
open ModuleTypes

type term = DataTypes.term
type cterm = DataTypes.cterm
type repl = DataTypes.repl

let fprint_cterm = SimplePrettyPrinter.fprint_cterm

let parse_term = Parsers.term
let parse_repl = Parsers.repl

module rec SimpleEnv : Env.T
	with type value = SimpleNormalizer.value_t
	and type t = SimpleNormalizer.value_t Env.env_t
	= Env.Make (SimpleNormalizerEnv) (SimpleTypeofEnv)
and SimpleNormalizerEnv : Evaluator
	with type env = SimpleNormalizer.value_t Env.env_t
	and type value = SimpleNormalizer.value_t
	= SimpleNormalizer.Make (SimpleEnv)
and SimpleTypeofEnv : Typeof
	with type env = SimpleNormalizer.value_t Env.env_t
	= SimpleTypeof.Make (SimpleEnv) (SimpleNormalizerEnv) (SimpleAlphaEquivChecker)

module SimpleUniquifierEnv = SimpleUniquifier.Make (SimpleEnv)
module SimpleTypeCheckerEnv = SimpleTypeChecker.Make (SimpleEnv) (SimpleNormalizerEnv) (SimplePrettyPrinter) (SimpleAlphaEquivChecker)
module SimpleCheckerEnv = SimpleChecker.Make (SimpleEnv) (SimpleUniquifierEnv) (SimpleTypeCheckerEnv)
module DefRunnerEnv = DefRunner.Make (SimpleEnv) (SimpleCheckerEnv)
module SimpleExecutorEnv = SimpleExecutor.Make (SimpleEnv) (SimpleCheckerEnv) (SimpleNormalizerEnv)
module ReplRunnerEnv = ReplRunner.Make (SimpleEnv) (SimpleCheckerEnv) (DefRunnerEnv) (SimplePrettyPrinter) (SimpleTypeofEnv) (SimpleNormalizerEnv) (SimpleExecutorEnv)

type env = SimpleEnv.t
type prog = SimpleExecutorEnv.prog

let normalize = SimpleNormalizerEnv.normalize
and evaluate = SimpleNormalizerEnv.evaluate
and typeof = SimpleTypeofEnv.typeof
and uniquify = SimpleUniquifierEnv.uniquify
and type_check = SimpleTypeCheckerEnv.type_check
and check_of_type = SimpleTypeCheckerEnv.check_of_type
and check = SimpleCheckerEnv.check
and run_repl = ReplRunnerEnv.run_repl
and run_prog = SimpleExecutorEnv.exec
and check_prog = SimpleExecutorEnv.check

let default_env =
	let check_add name term env = match check env term with
	| Ok c_t -> SimpleEnv.add (gen_ident name) c_t env
	| Error msg -> raise (Failure (sprintf "Check Error in default_env: %s" msg))
	in let parse_def s = match Parsers.term (Stream.of_string s) with
	| None -> raise (Failure (sprintf "Parse Error in default_env in `%s`" s))
	| Some t -> t
	in let rec add_defs env = function
	| [] -> env
	| ((name, s) :: rest) -> add_defs (check_add name s env) rest
	in let float_op op = Lam ("a", Some FloatT, Lam ("b", Some FloatT, FloatOp (op, Var "a", Var "b")))
	in let defs = [
		("id", parse_def "\\t : Type. \\x : t. 10");
		("true", Bool true);
		("false", Bool false);
		("add", float_op (Num Add));
		("sub", float_op (Num Sub));
		("mul", float_op (Num Mul));
		("div", float_op (Num Div));
		("eq", float_op (Cmp Eq));
		("neq", float_op (Cmp Lte));
		("lt", float_op (Cmp Lt));
		("lte", float_op (Cmp Lte));
		("gt", float_op (Cmp Gt));
		("gte", float_op (Cmp Gte));
		("left", Lam ("l", Some Star, Lam ("r", Some Star, Lam ("t", Some (PairT (Var "l", Var "r")), Left (Var "t")))));
		("right", Lam ("l", Some Star, Lam ("r", Some Star, Lam ("t", Some (PairT (Var "l", Var "r")), Right (Var "t")))));
		("printFloat", Lam ("f", Some FloatT, Lam ("w", Some RealWorldT, Action (PrintFloat, Var "w", [Var "f"]))));
		("IO", parse_def "\\t : Type. /w : RealWorld. (t ^ RealWorld)");
		("seqIO", parse_def "\\a : IO Unit. \\b : IO Unit. \\w : RealWorld. (\\w : RealWorld. b w) (right Unit RealWorld (a w))");
	]
	in add_defs SimpleEnv.empty defs
