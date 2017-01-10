open Types
open Prover
open Input

(*Algorithm 1*)
let build_state class_inv a actions =
	let initial_actions = List.filter
		(fun action -> Prover.prove ["(assert "^class_inv^")"; "(assert "^a^")"; "(assert "^action.action_requires^")"]) 
		actions in
			{actions = initial_actions; is_choice_state = false}

let generate_choice_state action =
	let dummy_action = {action_name = (action.action_name^"_choice"); 
		action_type = "unit";
		action_usage_state = -1;
		action_requires = "";
		action_ensures = "";
		is_sync = false; 
		parameters = []; 
		action_body = []} in {actions = [dummy_action]; is_choice_state = true}


let build_transitions class_inv class_init actions action_a state_a =
	if action_a.action_type = "boolean" && Prover.has_two_states action_a
	then
		let next_state_true = (build_state class_inv (Prover.get_true action_a.action_ensures) actions) and 
		    next_state_false = (build_state class_inv (Prover.get_false action_a.action_ensures) actions) in
			[{state_a = state_a; transition_label = action_a.action_name; state_b = generate_choice_state action_a};{state_a = generate_choice_state action_a; transition_label = "true"; state_b = next_state_true}; {state_a = generate_choice_state action_a; transition_label = "false"; state_b = next_state_false}]
	else 
		if (not (Prover.changes_state action_a))
		then [{state_a = state_a; transition_label = action_a.action_name; state_b = state_a}]
		else let next_state = (build_state class_inv action_a.action_ensures actions) in
			[{state_a = state_a; transition_label = action_a.action_name; state_b = next_state}]
			

let rec analyse_state class_inv class_init actions w states delta =
	match w with
		[] -> delta
		| state::w -> 
			if (not (List.exists (fun s -> s = state) states) && not state.is_choice_state) 
				then let transitions = List.flatten (List.map
					(fun a -> build_transitions class_inv class_init actions a state)
				state.actions) in
					analyse_state class_inv class_init actions (List.append w (List.map (fun t -> t.state_b) transitions)) (List.append states [state]) (List.append delta transitions)
				else analyse_state class_inv class_init actions w states delta



let generate_typestate class_inv class_init actions =
	let initial_state = (build_state class_inv class_init actions) in
		analyse_state class_inv class_init actions [initial_state] [] []
		
(*algorithm 2*)
let rec build_state_set states analysed_states =
	match states with
		[] -> analysed_states
		| state::states -> let is_analysed = List.exists (fun s -> s.state = state) analysed_states in
			if is_analysed then build_state_set states analysed_states
			else build_state_set states (List.append analysed_states [{id = (List.length analysed_states); state = state}])

let rec get_shared_status usage_state_id branches =
	List.for_all
	(fun b -> List.length b.next_states < 2 && List.hd b.next_states = usage_state_id)
	branches

let get_next_state state action delta =
	List.hd (List.map (fun t -> t.state_b) (List.filter (fun t -> t.state_a = state && t.transition_label = action.action_name) delta))

let get_decision_transition state_a label delta =
	List.find (fun t -> t.state_a = state_a && t.transition_label = label) delta

let generate_branches analysed_state analysed_states state delta =
	List.map 
	(fun a -> let next_state = get_next_state state a delta in 
		if next_state.is_choice_state 
			then let true_transition = get_decision_transition next_state "true" delta and false_transition = get_decision_transition next_state "false" delta in 
					({action = a; next_states = [(List.find (fun s -> s.state = true_transition.state_b) analysed_states).id; (List.find (fun s -> s.state = false_transition.state_b) analysed_states).id]} )
			else if next_state.actions = [] then {action = a; next_states = [-1]} 
			else {action = a; next_states = [(List.find (fun s -> s.state = next_state) analysed_states).id]}) state.actions

let rec generate_usage analysed_states states delta usage =
	match states with
		[] -> usage
		| state::states -> let analysed_state = List.find (fun s -> s.state = state) analysed_states in
			let branches = generate_branches analysed_state analysed_states state delta in			
			List.append 
			[{
				usage_state_id = analysed_state.id;
				usage_state_shared = get_shared_status analysed_state.id branches;
				branches = branches
			}]
			(generate_usage analysed_states states delta usage)

let rec annotate_classes input_classes output_classes =
	match input_classes with
		[] -> output_classes
		| c::input_classes -> 
			let delta = generate_typestate c.class_inv c.class_init (List.filter (fun action -> action.action_name <> c.class_name) c.actions) in 
				let states = List.filter (fun s -> not s.is_choice_state) (List.map (fun t -> t.state_a) delta) in 
					let analysed_states = (build_state_set states []) in
						let usage = (generate_usage analysed_states (List.map (fun s -> s.state) analysed_states) delta []) in 
							let new_class = {class_name = c.class_name; class_usage = usage; class_fields = c.class_fields; class_inv = c.class_inv; class_init = c.class_init; actions = c.actions} in
								annotate_classes input_classes (List.append output_classes [new_class])

(*algorithm 3*)
let is_primitive_type t =
	match t with
		"void" -> true
		| "boolean" -> true
		| "int" -> true
		| "string" -> true
		| _ -> false

let rec get_parameter_usage_state_2 usage action =
	match usage with
			[] -> -1
			| usage_state::usage -> if (List.for_all (fun b -> Prover.prove [("(assert "^action.action_requires^")"); ("(assert "^b.action.action_requires^")")]) usage_state.branches) then usage_state.usage_state_id else get_parameter_usage_state_2 usage action

let rec get_parameter_usage_state_1 classes parameter action =
	match is_primitive_type parameter.var_type with
		true -> -1
		| false -> let usage = (List.find (fun c -> c.class_name = parameter.var_type) classes).class_usage in
			get_parameter_usage_state_2 usage action

let rec annotate_parameters classes action input_parameters output_parameters =
	match input_parameters with
		[] -> output_parameters
		| p::input_parameters -> 
			let new_parameter = {var_name = p.var_name; var_type = p.var_type; var_usage_state = (get_parameter_usage_state_1 classes p action)} in
				annotate_parameters classes action input_parameters (List.append output_parameters [new_parameter])

let rec get_action_usage_state_2 usage action =
	match usage with
			[] -> -1
			| usage_state::usage -> if (List.for_all (fun b -> Prover.prove [("(assert "^action.action_ensures^")"); ("(assert "^b.action.action_requires^")")]) usage_state.branches) then usage_state.usage_state_id else get_action_usage_state_2 usage action

let rec get_action_usage_state_1 classes action =
	match is_primitive_type action.action_type with
		true -> -1
		| false -> let usage = (List.find (fun c -> c.class_name = action.action_type) classes).class_usage in
			get_action_usage_state_2 usage action

let rec annotate_actions classes input_actions output_actions =
	match input_actions with
		[] -> output_actions
		| a::input_actions -> 
			let new_parameters = annotate_parameters classes a a.parameters [] in
				let new_action = {action_name = a.action_name; action_usage_state = (get_action_usage_state_1 classes a); action_type = a.action_type; action_requires = a.action_requires; action_ensures = a.action_ensures; is_sync = a.is_sync; parameters = new_parameters; action_body = a.action_body} in
			annotate_actions classes input_actions (List.append output_actions [new_action])

let get_field_usage_state_from_param parameters param_name =
	let p = List.hd (List.filter (fun f -> f.var_name = param_name) parameters) in
		p.var_usage_state

let get_field_usage_state_from_call classes actions class_name action_name =
	let c = List.find (fun c -> c.class_name = class_name) classes in
		let a = List.hd (List.filter (fun a -> a.action_name = action_name) c.actions) in
			a.action_usage_state

let rec detect_attributions_2 classes fields actions parameters s output_fields=
	match s with
		{stmt_type = ASSIGN; left_side = [s1]; right_side = [{stmt_type = FIELD; left_side = []; right_side = []; value = v}]} ->
			let field = List.find (fun f -> f.var_name = s1.value) fields in 
				output_fields@[{var_name = field.var_name; var_type = field.var_type; var_usage_state = (get_field_usage_state_from_param parameters v)}]
		| {stmt_type = ASSIGN; left_side = [s1]; right_side = [{stmt_type = CALL; left_side = [s2]; right_side = [s3]; value = v}]} ->
			let field = List.find (fun f -> f.var_name = s1.value) fields in 
				output_fields@[{var_name = field.var_name; var_type = field.var_type; var_usage_state = (get_field_usage_state_from_call classes actions s2.value s3.value)}]
		| {stmt_type = IF; left_side = [s1]; right_side = []} -> detect_attributions_2 classes fields actions parameters s1 output_fields
		| {stmt_type = IFELSE; left_side = [s1]; right_side = [s2]} -> detect_attributions_2 classes fields actions parameters s2 (detect_attributions_2 classes fields actions parameters s1 output_fields)
		| {stmt_type = SEQ; left_side = [s1]; right_side = [s2]} -> detect_attributions_2 classes fields actions parameters s2 (detect_attributions_2 classes fields actions parameters s1 output_fields)
		| {stmt_type = SPAWN; left_side = [s1]} -> detect_attributions_2 classes fields actions parameters s1 output_fields
		| {stmt_type = WHILE; left_side = [s1]; right_side = []} -> detect_attributions_2 classes fields actions parameters s1 output_fields
		| _ -> output_fields


let rec detect_attributions_1 classes fields actions parameters stmts output_fields =
	match stmts with
		[] -> output_fields
		| s::stmts -> let new_output_fields = detect_attributions_2 classes fields actions parameters s [] in
			detect_attributions_1 classes fields actions parameters stmts new_output_fields

let rec annotate_fields classes fields actions output_fields =
	match actions with
		[] -> output_fields
		| a::actions -> detect_attributions_1 classes fields actions a.parameters a.action_body [];
				output_fields

let rec get_object_usage_states input_classes output_classes =
	match input_classes with
		[] -> output_classes
		| c::input_classes -> 
			let new_actions = annotate_actions (input_classes@output_classes@[c]) c.actions [] in
				let new_fields = annotate_fields (input_classes@output_classes@[c]) c.class_fields new_actions [] in
					let new_class = {class_name = c.class_name; class_usage = c.class_usage; class_fields = new_fields; class_inv = c.class_inv; class_init = c.class_init; actions = new_actions} in
						get_object_usage_states input_classes (List.append output_classes [new_class])

(*Main*)
let main = 
	let annotated_classes = annotate_classes classes [] in
		get_object_usage_states annotated_classes []
