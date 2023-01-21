open DataTypes
open Opal

let reserved = [ "if"; "then"; "else"; "let"; "letrec"; "in"; "Float"; "Bool"; "Unit"; "RealWorld"; "Type" ]

let parse_ident = (spaces >> letter <~> many alpha_num) => implode >>= function
| s when List.mem s reserved -> mzero
| s -> return s

let parse_term input =
	let rec float_ input =
		( many1 digit >>= fun digits ->
		  return (Float (Float.of_string (implode digits)))
		) input
	and num_t input = ( token "Float" >> return FloatT ) input
	and bool_t input = ( token "Bool" >> return BoolT ) input
	and unit_ input = ( token "(" >> spaces >> token ")" >> return Unit) input
	and unit_t input = ( token "Unit" >> return UnitT ) input
	and real_world_t input = ( token "RealWorld" >> return RealWorldT ) input
	and star input = ( token "Type" >> return Star ) input
	and var input = ( parse_ident >>= fun v -> return (Var v)) input
	and lam input =
		( token "\\" >>
		  spaces >>
		  parse_ident >>= fun v ->
		  ( ( spaces >>
		      token ":" >>
		      spaces >>
		      term >>= fun v_t ->
		      return (Some (v_t))
		    ) <|> return None
		  ) >>= fun v_t ->
		  spaces>>
		  token "." >>
		  spaces >>
		  term >>= fun b ->
		  return (Lam (v, v_t, b))
		) input
	and pi input =
		( token "/" >>
		  spaces >>
		  parse_ident >>= fun v ->
		  spaces >>
		  token ":" >>
		  spaces >>
		  term >>= fun v_t ->
		  spaces>>
		  token "." >>
		  spaces >>
		  term >>= fun b ->
		  return (Pi (v, v_t, b))
		) input
	and app input =
		( sep_by1 arg spaces >>= fun (f_args) ->
		  return (List.fold_left (fun f a -> App (f, a)) (List.hd f_args) (List.tl f_args))
		) input
	and if_ input =
		( token "if" >>
		  spaces >>
		  term >>= fun p ->
		  spaces >>
		  token "then" >>
		  spaces >>
		  term >>= fun c ->
		  spaces >>
		  token "else" >>
		  spaces >>
		  term >>= fun a ->
		  return (If (p, c, a))
		) input
	and pair input =
		( token "(" >>
		  spaces >>
		  term >>= fun l ->
		  spaces >>
		  token "," >>
		  spaces >>
		  term >>= fun r ->
		  spaces >>
		  token ")" >>
		  return (Pair (l, r))
		) input
	and pair_t input =
		( token "(" >>
		  spaces >>
		  term >>= fun l ->
		  spaces >>
		  token "^" >>
		  spaces >>
		  term >>= fun r ->
		  spaces >>
		  token ")" >>
		  return (PairT (l, r))
		) input
	and arg input = (float_ <|> var <|> num_t <|> bool_t <|> unit_t <|> real_world_t <|> star <|> pair <|> pair_t <|> unit_ <|> parens) input
	and ann input =
		( arg >>= fun t ->
		  spaces >>
		  token ":" >>
		  spaces >>
		  term >>= fun t_t ->
		  return (Ann (t, t_t))
		) input
	and let_ input =
		( token "let" >>
		  spaces >>
		  parse_ident >>= fun var ->
		  spaces >>
		  token "=" >>
		  spaces >>
		  term >>= fun val_ ->
		  spaces >>
		  token "in" >>
		  spaces >>
		  term >>= fun body ->
		  return (Let (var, val_, body))
		) input
	and letrec input =
		( token "letrec" >>
		  spaces >>
		  parse_ident >>= fun var ->
		  spaces >>
		  token ":" >>
		  spaces >>
		  term >>= fun var_t ->
		  spaces >>
		  token "=" >>
		  spaces >>
		  term >>= fun val_ ->
		  spaces >>
		  token "in" >>
		  spaces >>
		  term >>= fun body ->
		  return (Letrec (var, var_t, val_, body))
		) input
	and parens input =
		( token "(" >>
		  spaces >>
		  term >>= fun t ->
		  spaces >>
		  token ")" >>
		  return t
		) input
	and term input = (ann <|> let_ <|> letrec <|> lam <|> pi <|> if_ <|> app) input
	in term input

let rec parse_repl input =
	let rec let_ input =
		( token ":let" >>
		  spaces >>
		  parse_ident >>= fun v ->
		  spaces >>
		  token "=" >>
		  spaces >>
		  parse_term >>= fun t ->
		  return (RLet (v, t))
		) input
	and exec input =
		( token ":exec" >>
		  spaces >>
		  parse_term >>= fun t ->
		  return (RExec t)
		) input
	and load input =
		( token ":load " >>
		  many (satisfy (fun c -> not (c == '\n'))) >>= fun char_list ->
		  return (RLoad (implode char_list))
		) input
	and term input =
		( spaces >>
		  parse_term >>= fun t ->
		  return (RNorm t)
		) input
	and repl input = (let_ <|> exec <|> load <|> term) input
	in repl input

let parse_def input =
	let let_ input =
		( token "let" >>
		  spaces >>
		  parse_ident >>= fun var ->
		  spaces >>
		  ( ( token ":" >>
		      parse_term >>= fun type_ ->
		      spaces >>
		      return (Some type_)
		    ) <|> return None
		  ) >>= fun type_ -> 
		  token "=" >>
		  spaces >>
		  parse_term >>= fun val_ ->
		  return (DLet (var, type_, val_))
		) input
	in let_ input

let parse_defs input = many (spaces >> parse_def) input

let stream_parse p s = Opal.parse p (LazyStream.of_stream s)

let repl = stream_parse parse_repl
let term = stream_parse parse_term
let def = stream_parse parse_def
let defs = stream_parse parse_defs
