open Printf
open Types
open Input

open Unix

let file = "assertions.z3"
let file2 = "assertions2.z3"
let file3 = "assertions3.z3"

let print_assertions_to_file file_channel assertions =
	(*Declare fields*)
	List.iter
	(fun field -> fprintf file_channel "%s\n" field)
	fields;
	(*Declare assertions*)
	List.iter
	(fun assertion -> fprintf file_channel "%s\n" assertion)
	assertions; 
	fprintf file_channel "%s\n" "(check-sat)"

let prove assertions =
	let file_channel = open_out file in
		print_assertions_to_file file_channel assertions; close_out file_channel;
	let res = (input_line (Unix.open_process_in "z3 assertions.z3")) in
		if res = "sat" then true else false

let prove_variant assertions =
	let file_channel = open_out file2 in
		print_assertions_to_file file_channel assertions; close_out file_channel;
	let res = (input_line (Unix.open_process_in "z3 assertions2.z3")) in
		Sys.remove "assertions2.z3"; if res = "sat" then true else false

let rec build_assertion channel next_line =
	match next_line with
		"  :precision precise :depth 1)" -> ""
		| "(error \"tactic failed: split-clause tactic failed, goal does not contain any clause\")
" -> print_string "ERROR\n"; ""
		| line -> line^(build_assertion channel (input_line channel))

let get_true assertion =
	let file_channel = open_out file2 in
		List.iter
		(fun field -> fprintf file_channel "%s\n" field)
		fields;
		fprintf file_channel "%s\n" ("(assert "^assertion^")");

		fprintf file_channel "%s\n" "(apply split-clause)"; close_out file_channel;
		let channel = (Unix.open_process_in "z3 assertions2.z3") in
			input_line channel; input_line channel;
			"(and "^(build_assertion channel "")^")"

let get_false assertion =
	let file_channel = open_out file2 in
		List.iter
		(fun field -> fprintf file_channel "%s\n" field)
		fields;
		fprintf file_channel "%s\n" ("(assert "^assertion^")");

		fprintf file_channel "%s\n" "(apply split-clause)"; close_out file_channel;
		let channel = (Unix.open_process_in "z3 assertions2.z3") in
			input_line channel; input_line channel; (build_assertion channel ""); input_line channel;
			"(and "^(build_assertion channel "")^")"

let get_variant assertion state =
	let file_channel = open_out file2 in
		List.iter
		(fun field -> fprintf file_channel "%s\n" field)
		fields;
		fprintf file_channel "%s\n" assertion;

		fprintf file_channel "%s\n" "(apply split-clause)"; close_out file_channel;
		let channel = (Unix.open_process_in "z3 assertions2.z3") in
			input_line channel; input_line channel;
		
			let assertion_left = "(assert (and "^(build_assertion channel "")^"))\n" in
				input_line channel;
				let assertion_right = "(assert (and "^(build_assertion channel "")^"))\n" in
					List.exists
					(fun action -> (prove_variant [assertion_left; "(assert "^action.action_requires^")"]) && (not (prove_variant [assertion_right; "(assert "^action.action_requires^")"])))
					state

let has_two_states action =
	String.sub action.action_ensures 0 3 = "(or"

let changes_state action =
	if(action.action_requires = action.action_ensures) 
		then false
		else
			(let file_channel = open_out file3 in
				List.iter
				(fun field -> fprintf file_channel "%s\n" field)
				fields;
				fprintf file_channel "%s\n" ("(assert "^action.action_ensures^")");

				fprintf file_channel "%s\n" "(apply split-clause)"; close_out file_channel;
				let channel = (Unix.open_process_in "z3 assertions3.z3") in
					if ((input_line channel) = "(error \"tactic failed: split-clause tactic failed, goal does not contain any clause\")")
					then not (prove ["(assert "^(action.action_requires)^")"; "(assert "^action.action_ensures^")"])
					else (input_line channel;
					let assertion_left = "(assert (and "^(build_assertion channel "")^"))\n" in
						input_line channel;
						let assertion_right = "(assert (and "^(build_assertion channel "")^"))\n" in
							not (prove ["(assert "^(action.action_requires)^")"; assertion_left]) || not (prove ["(assert "^(action.action_requires)^")"; assertion_right])))
