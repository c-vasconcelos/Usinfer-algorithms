open Types

let unit_stmt = {
	stmt_type = UNIT; 
	left_side = []; 
	right_side = []; 
	value = "unit"
}

let true_stmt = {
	stmt_type = TRUE; 
	left_side = []; 
	right_side = []; 
	value = "true"
}

let int_stmt = {
	stmt_type = INTEGER; 
	left_side = []; 
	right_side = []; 
	value = "0"
}

(*Class File*)
let a_file = {
	action_name = "File"; 
	action_usage_state = -1;
	action_type = "void"; 
	action_requires = "true"; 
	action_ensures = "(and (= linesRead 0) (= linesInFile 5) (= closed false) (= lineInBuffer false))"; 
	is_sync = false; 
	parameters = [];
	action_body = [unit_stmt]};;

let a_read = {
	action_name = "read"; 
	action_usage_state = -1;
	action_type = "string"; 
	action_requires = "(and (< linesRead linesInFile) (= closed false) (= lineInBuffer true))"; 
	action_ensures = "(and (<= (+ linesRead 1) linesInFile) (= closed false) (= lineInBuffer false))"; 
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_eof = {
	action_name = "eof"; 
	action_usage_state = -1;
	action_type = "boolean"; 
	(*action_type = "unit"; *)
	action_requires = "(and (< linesRead linesInFile) (not closed) (not lineInBuffer))";
	action_ensures = "(or (and (= linesRead linesInFile) (not lineInBuffer) (not closed)) (and (not (= linesRead linesInFile)) lineInBuffer (not closed)))";
	(*action_ensures = "(and (= linesRead linesInFile) (not lineInBuffer) (not closed))";  *)
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_close = {
	action_name = "close"; 
	action_usage_state = -1;
	action_type = "void"; 
	action_requires = "(and (= linesRead linesInFile) (= closed false))"; 
	action_ensures = "(and (= linesRead linesInFile) (= closed true))"; 
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let class_file = {
	class_name = "File"; 
	class_usage = [];
	class_fields = []; 
	class_inv = "(and (>= linesRead 0) (<= linesRead linesInFile))"; 
	class_init = "(and (= linesRead 0) (= linesInFile 5) (= closed false) (= lineInBuffer false))"; 
	actions = [a_file;a_read;a_eof;a_close]
}

(*Class FileReader*)
let a_filereader = {
	action_name = "FileReader"; 
	action_usage_state = -1;
	action_type = "void"; 
	action_requires = "f"; 
	action_ensures = "(and (= counter 0) (= linesRead 0) (= linesInFile 5) (= closed false) (= lineInBuffer false) (not isEof))"; 
	is_sync = false; 
	parameters = [{var_name = "f"; var_type = "File"; var_usage_state = -1}]; 
	action_body = [{stmt_type = ASSIGN; left_side = [{stmt_type = FIELD; left_side = []; right_side = []; value = "file"}]; right_side = [{stmt_type = FIELD; left_side = []; right_side = []; value = "f"}]; 
	value = "field = f"}]
}

let a_next = {
	action_name = "next"; 
	action_usage_state = -1;
	action_type = "boolean"; 
	action_requires = "(not isEof)"; 
	action_ensures = "(or (not isEof) isEof)"; 
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_tostring = {
	action_name = "toString"; 
	action_usage_state = -1;
	action_type = "string"; 
	action_requires = "true"; 
	action_ensures = "true"; 
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_getcounter = {
	action_name = "getCounter";
	action_usage_state = -1; 
	action_type = "int"; 
	action_requires = "isEof"; 
	action_ensures = "isEof"; 
	is_sync = false; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_count = {
	action_name = "count"; 
	action_usage_state = -1;
	action_type = "void"; 
	action_requires = "false"; 
	action_ensures = "false"; 
	is_sync = true; 
	parameters = []; 
	action_body = [unit_stmt]
}

let a_getfile = {action_name = "getFile"; 
	action_usage_state = -1; action_type = "File"; action_requires = "isEof"; action_ensures = "(and isEof f)"; is_sync = true; parameters = []; action_body = [unit_stmt]};;

let class_filereader = {
	class_name = "FileReader"; 
	class_usage = [];
	class_fields = [{var_name = "file"; var_type = "File"; var_usage_state = -1}; {var_name = "isEof"; var_type = "boolean"; var_usage_state = -1}]; 
	class_inv = "(>= counter 0)"; 
	class_init = "(and (= counter 0) (= linesRead 0) (= linesInFile 5) (= closed false) (= lineInBuffer false) (not isEof))"; 
	actions = [a_filereader;a_next;a_tostring;a_getcounter;a_count;a_getfile]
}

let classes = [class_file; class_filereader]

let fields = ["(declare-const linesRead Int)"; "(declare-const linesInFile Int)"; "(declare-const closed Bool)"; "(declare-const lineInBuffer Bool)"; "(declare-const counter Int)"; "(declare-const isEof Bool)"; "(declare-const f Bool)"]
