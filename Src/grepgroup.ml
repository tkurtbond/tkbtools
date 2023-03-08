(* grepgroup.ml -- print the specified groups from a regexp. *)

(* Todo:
   1. Print out all matches on a line. 
*)

let revision_id = "$Id: grepgroup.ml 1.1 Tue, 16 Apr 2002 22:25:24 -0400 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0
    
let rx_string = ref None
and rx = ref (Str.regexp "x")		(* placeholder *)
and filenames = ref []
and groups = ref []
and print_group = ref false
and print_name = ref false
and print_lineno = ref false
and print_line = ref false
and found = ref false
and ignore_case = ref false
and sep = ref "|"
and separate = ref true

let process_arg arg =
  match !rx_string with
  | None -> rx_string := Some arg
  | Some x -> filenames := arg :: !filenames

let soi = string_of_int 

let unwind_protect fn exitfn =
  let rslt = try fn() with exn -> ignore(exitfn()); raise exn
  in ignore(exitfn()); rslt

let matches = ref ([] : (int * string) list)

let accumulate_match line group =
  try 
    let m = Str.matched_group group line in 
    matches := (group, m) :: !matches
  with Not_found -> ()

let print_match filename lineno (group, s) =
  print_endline ((if !print_name then filename ^ ":" else "") ^
		 (if !print_lineno then (soi lineno) ^ ":" else "") ^
		 (if !print_group then "(" ^ (soi group) ^ "): " else "") ^
		 s);
  flush stdout

let all = ref ""

let string_match (group, s) =
  if !all <> "" then all := !all ^ !sep;
  all := !all ^ s

let print_matches filename lineno =
  let matches = List.rev !matches in
  if !separate then
    List.iter (print_match filename lineno) matches
  else begin
    List.iter string_match matches;
    print_endline ((if !print_name then filename ^ ":" else "") ^
		   (if !print_lineno then (soi lineno) ^ ":" else "") ^
		   !all)
  end
    

let process_chan filename chan =
  try
    let lineno = ref 0 in 
    while true do 
      let line = input_line chan in
      incr lineno;
      matches := [];
      all := "";
      begin 
	try
	  ignore (Str.search_forward !rx line 0);
	  if not !found then found := true;
	  List.iter (accumulate_match line) !groups;
	  print_matches filename !lineno;
	  if !print_line then print_endline ("^^^ " ^ line);
	  flush stdout;
	with Not_found -> ()
      end
    done
  with End_of_file -> ()
    

let process_filename filename =
  try 
    let chan = open_in filename in
    process_chan filename chan;
    close_in chan
  with Sys_error err -> prerr_endline ("grepgroup:" ^ filename ^ ": " ^ err)
  

let add_group group = groups := group :: !groups


let main () = 
  let argdefs = [
    ("-g", Arg.Int add_group, "group\tAdd group to the groups to print");
    ("-i", Arg.Set ignore_case, "\tIgnore case");
    ("-n", Arg.Set print_lineno, "\tPrint line number");
    ("-H", Arg.Set print_name, "\tPrint the name of the file");
    ("-l", Arg.Set print_line, "\tPrint the line following the matches");
    ("-p", Arg.Set print_group, "\tPrint group number before group");
    ("-S", Arg.Clear separate, "\tPrint on same line");
    ("-sep", Arg.String ((:=) sep),
     "string\tSeparator string for printing on same line");
    ("-version", Arg.Unit print_revision,
     "\tPrint version info and exit");
  ] in
  Arg.parse argdefs process_arg "usage: grepgroup [options] regexp file [...]";
  begin match !rx_string with
  | None -> 
    prerr_endline "grepgroup: must specify a regular expression";
    exit 2;
  | Some s ->
      rx := (if !ignore_case then Str.regexp_case_fold else Str.regexp) s;
  end;
  if (List.length !groups) = 0 then begin
    prerr_endline "grepgroup: must specify one or more groups to print";
    exit 2;
  end;
  groups := List.rev !groups;
  if (List.length !filenames) = 0 then
    process_chan "(stdin)" stdin
  else
    List.iter process_filename (List.rev !filenames);
  exit (if !found then 0 else 3)

let _ = 
  Printexc.catch main ()
