open String

let changecase = ref lowercase_ascii

let strings = ref []

let add_string s = strings := s :: !strings 

let process strings =
  let rec iter strings first =
    match strings with
    | s :: rest ->
	if not first then print_char ' ';
	print_string (!changecase s);
	iter rest false
    | [] -> print_newline (); ()
  in iter strings true

let main () =
  let specs =
  [ ("-c", Arg.Unit (fun () -> changecase := capitalize_ascii),
     "capitalize words");
    ("-l", Arg.Unit (fun () -> changecase := lowercase_ascii),
     "lowercase words");
    ("-u", Arg.Unit (fun () -> changecase := uppercase_ascii),
     "uppercase words") ]
  in
  Arg.parse specs add_string "usage: changecase [-c|-l|-u] words [...]";
  process (List.rev !strings)

let _ =
  Printexc.print main ()
