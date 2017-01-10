type stmt_type = 
	AND 
	| ASSIGN 
	| CALL 
	| CONCAT
	| DIFF
	| DIV
	| EQUAL
	| FALSE
	| FIELD
	| GREATER
	| GREATEROREQUAL
	| IF
	| IFELSE
	| INTEGER
	| LESS
	| LESSOREQUAL
	| MINUS
	| MULT
	| NEW
	| NOT
	| NULL
	| OR
	| PLUS
	| PRINTINT
	| PRINTSTR
	| SEQ
	| SPAWN
	| STRING
	| TRUE
	| UNIT
	| WHILE	

type statement = {
	stmt_type : stmt_type; 
	left_side : statement list; 
	right_side : statement list; 
	value : string
}

type var = {
	var_name : string; 
	var_type : string;
	var_usage_state : int
}

type action = {
	action_name : string; 
	action_usage_state : int;
	action_type : string; 
	action_requires : string; 
	action_ensures : string; 
	is_sync : bool; 
	parameters : var list; 
	action_body : statement list
}

type state = {
	actions : action list;
	is_choice_state : bool
}

type transition = {
	state_a : state; 
	transition_label : string; 
	state_b : state
}

type usage_branch = {
	action : action; 
	next_states : int list
} and usage_state = {
	usage_state_id : int; 
	usage_state_shared : bool; 
	branches : usage_branch list
}

type mool_class = {
	class_name : string; 
	class_usage : usage_state list;
	class_fields : var list; 
	class_inv : string; 
	class_init : string; 
	actions : action list
}

type analyzed_state = {
	id : int;
	state : state
}
(*
type var_2 = {
	var_name : string; 
	var_type : string;
	var_usage_state : int
}

type action_2 = {
	action_name : string; 
	action_type : string; 
	action_requires : string; 
	action_ensures : string; 
	is_sync : bool; 
	parameters : var list; 
	action_body : statement list;
	action_usage_state : int
}

type annotated_mool_class = {
	class_name : string; 
	usage : usage_state list;
	class_fields : var list; 
	class_inv : string; 
	class_init : string; 
	actions : action list
}*)
