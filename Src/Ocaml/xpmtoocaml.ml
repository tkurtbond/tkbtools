(* xpmtoocaml.ml -- convert a C format xpm file into Objective Caml. *)

(* Warning: this is crude and unfinished. *)

let program_name = Sys.argv.(0)

let name_rx = Str.regexp "static char \\* \\([A-Za-z0-9_]+\\)\\[\\] = {"
let line_rx = Str.regexp "[ \t]*\"\\(.*\\)\"\\(,\\|};\\)[ \t]*"

let process_file infile =
  let s = input_line infile in
  if s <> "/* XPM */" then
    raise (Failure ("Invalid line 1: " ^ s));
  let s = input_line infile in
  if not (Str.string_match name_rx s 0) then
    raise (Failure ("Invalid line 2: " ^ s));
  let pixmap_name = Str.matched_group 1 s in
  Printf.printf "let %s = \n  [|" pixmap_name;
  try
    let lineno = ref 2 in 
    while true do
      let s = input_line infile in
      incr lineno;
      if not (Str.string_match line_rx s 0) then
	raise (Failure ("Invalid line " ^ (string_of_int !lineno) ^ ": " ^ s));
      let s = Str.matched_group 1 s in 
      Printf.printf "%s\"%s\"" (if !lineno > 3 then ";\n   " else "") s;
    done
  with End_of_file ->
    Printf.printf "|]\n\n"

let process_filename filename =
  try 
    let file = open_in filename in
    Printf.printf "(* origin: %s *)\n\n" filename;
    process_file file
  with Sys_error msg ->
    prerr_endline (program_name ^ ": " ^ msg)

let main () =
  let specs = [ ] in
  let usage = "usage: " ^ program_name ^ " [options] inputfile ..." in
  Printf.printf "(* automatically generated *)\n\n";
  Arg.parse specs process_filename usage
    
let _ = Printexc.print main ()
