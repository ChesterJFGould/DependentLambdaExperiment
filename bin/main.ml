open Format;;
open Opal;;
open Lib.Term;;

let rec repl env = 
	printf "λ> %!";
	match In_channel.input_line stdin with
	| None -> ()
	| Some s -> begin match parse_repl (Stream.of_string s) with
		| None ->
			print_endline "Parse Error";
			repl env
		| Some t ->
			begin match run_repl env t with
			| Error msg ->
				printf "Error: %s\n" msg;
				repl env
			| Ok new_env ->
				repl new_env
			end
		end

let () = repl default_env
