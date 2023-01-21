open ModuleTypes

module Make
	(Env : sig type t end)
	(C : Checker with type env = Env.t)
	(N : Normalizer with type env = Env.t)
	: sig
	include Executor with type env = Env.t
end
