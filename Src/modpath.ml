(* modpath.ml -- output a modified path.  

   Typically used something like
       eval $(modpath -delete /usr/local/bin -start /usr/public/bin \
                      -before /usr/ccs/bin /usr/gnu/bin)

   Or put something like 
	function repath () {
	    eval $(modpath "$@")
	}

   in the appropriate shell startup file, and do 
        modpath -delete /usr/local/bin -start /usr/public/bin \
               -before /usr/ccs/bin /usr/gnu/bin
*)

let revision_id = "1.11 2016-12-16"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let in_path_sep = ref (if Sys.os_type = "Win32" then ";" else ":")
let out_path_sep = ref !in_path_sep

let msys_style = ref false

(* Used in error messages; get from Sys.argv.(0) ??? *)
let progname = ref Sys.argv.(0)


type mode =				(* The default mode is Mode_end. *)
  | Mode_after of string		(* Add next anon arg after string *)
  | Mode_before of string		(* Add next anon arg before string *)
  | Mode_start				(* Add next anon arg at start *)
  | Mode_end				(* Add next anon arg at end *)

(* Controls what's to be done with the next anonymous argument.
   ??? Use Arg.current and manual access to Sys.argv instead??? *)
let mode = ref Mode_end


(* Variables and functions used by command line arguments.  *)


type output =
  | Out_nice				(* One segment per line *)
  | Out_simple				(* Colon separated *)
  | Out_cmd				(* As an NT cmd command *)
  | Out_csh				(* As a csh command *)
  | Out_sh				(* As a sh command *)
  | Out_quiet				(* Don't output it *)

(* How the new path should be output *)
let output = ref (if Sys.os_type = "Win32" then Out_cmd else Out_sh)

let set_output o () = output := o


(* path must be set by set_path or set_path_from_var, so that
   path_list gets set at the same time.  Note that no effort is made
   to keep path up to date with path_list.  *)
let path_var = ref "PATH"
let path = ref ""
let path_list = ref []

let warn_flag = ref false
let relative_flag = ref false
let exists_flag = ref false


(* Set both the input and output path separators.  *)
let set_sep s =
  in_path_sep := s;
  out_path_sep := s


(* Set path and then set path_list from path,
   given a string containing the path.  *)
let set_path s =
  path := s;
  path_list := (Str.split_delim (Str.regexp !in_path_sep) !path)

(* Set path and then set path_list from path, given the name of an environment
   variable.  It is a fatal error for the user to specify a non-existant
   environment variable, because if it wasn't the program would continue on
   operating on the *default* path, which is the value of PATH, and if one is
   trying to set LD_LIBRARY_PATH setting it to something based on PATH would
   be completely wrong. *)
let set_path_from_var var =
  path_var := var;
  try set_path (Sys.getenv var)
  with Not_found ->
    prerr_endline (!progname ^ ": " ^ (if !warn_flag then "warning" else "error")
		   ^ ": unable to get path from envrionment variable " ^ var);
    if !warn_flag then 
      set_path ""
    else
      exit 3


(* Warn if there's a mode that hasn't been taken care of yet.  *)
let check_mode mode =
  match mode with
  | Mode_after item ->
      prerr_endline (!progname ^ ": warning: unprocessed -after " ^ item)
  | Mode_before item ->
      prerr_endline (!progname ^ ": warning: unprocessed -before " ^ item)
  | Mode_start ->
      prerr_endline (!progname ^ ": warning: unprocessed -start")
  | Mode_end -> ()

	
(* Set the next anonymous argument to be added after 'item'.  *)
let set_add_after_mode item =
  check_mode !mode;
  mode := Mode_after item

(* Set the next anonymous argument to be added before 'item'.  *)
let set_add_before_mode item =
  check_mode !mode;
  mode := Mode_before item

(* Set the next anonymous argument to be added at the start of the path.  *)
let set_add_start_mode () =
  check_mode !mode;
  mode := Mode_start

(* Set the next anonymous argument to be added at the end of the path.  *)
let set_add_end_mode () =
  check_mode !mode;
  mode := Mode_end

(* Add 'item' to the end of the path.  *)
let add_end item =
  path_list := !path_list @ [item]

(* Add 'item' to the start of the path.  *)
let add_start item =
  path_list := item :: !path_list

(* Eliminate all duplicates in path_list.  *)
let unique () =
  let rec iter acc l =
    match l with
      [] -> acc
    | item :: rest ->
	if not (List.mem item acc) then
	  iter (acc @ [item]) rest
	else
	  iter acc rest
  in path_list := iter [] !path_list

(* Add 'item' after the part 'after' in the path.  *)
let add_after after item =
  let rec iter acc rest =
    match rest with
      [] ->
	prerr_endline (!progname ^ ": warning: " ^ after
		       ^ " is not in path to add " ^ item ^ " after it;"
		       ^ " adding at end");
	acc @ [item]
    | part :: rest ->
	if after = part then
	  acc @ [part; item] @ rest
	else
	  iter (acc @ [part]) rest
  in path_list := iter [] !path_list

(* Add 'item' before the part 'before' in the path.  *)
let add_before before item =
  let rec iter acc rest =
    match rest with
      [] ->
	prerr_endline (!progname ^ ": warning: " ^ before
		       ^ " is not in path to add " ^ item ^ " before it;"
		       ^ " adding at start");
	item :: acc
    | part :: rest ->
	if before = part then
	  acc @ [item; part] @ rest
	else
	  iter (acc @ [part]) rest
  in path_list := iter [] !path_list


(* Delete all occurrences of 'item' in the path.  *)
let delete item =
  let found = ref false in
  let rec iter acc rest =
    match rest with
      [] ->
	if not !found then
	  prerr_endline (!progname ^ ": warning: " ^ item ^ " is not in path to delete it");
	acc
    | part :: rest ->
	if item = part then begin
	  found := true;
	  iter acc rest
	end else
	  iter (acc @ [part]) rest
  in path_list := iter [] !path_list


(* Process each anonymous argument.  Note that this always sets 'mode' to
   the default so that at the end of processing arguments we can tell if
   there are any outstanding -after's or -before's.  *)
let anonymous_arg item =
  let item =
    if !relative_flag then item
    else if Filename.is_relative item then Filename.concat (Sys.getcwd ()) item
    else item
  in
  if (not !exists_flag) || (Sys.file_exists item) then begin
    match !mode with
    | Mode_after after -> add_after after item
    | Mode_before before -> add_before before item
    | Mode_start -> add_start item
    | Mode_end -> path_list := !path_list @ [item]
  end else begin
    prerr_endline (!progname ^ ": warning: pathname does not exist: " ^ item)
  end;
  exists_flag := false;
  mode := Mode_end

(* Add the current directory.  *)
let add_current () =
  let cwd = Sys.getcwd () in anonymous_arg cwd

(* For the cases where "" is just too easy to miss.  *)
let add_empty () = anonymous_arg ""

let is_letter c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let drive_rx = Str.regexp_case_fold "\\([a-z]\\):\\(.*\\)"
(* MSYS uses "/<drive-letter>" instead of "<drive-letter>:". *)
let msysize_path path_list =
  List.map
    (fun element ->
      let element = String.map (fun c -> if c = '\\' then '/' else c) element in
      if Str.string_match drive_rx element 0 then begin
	"/" ^ (Str.matched_group 1 element) ^ (Str.matched_group 2 element)
      end else element)
    path_list
      


(* Main processing.  *)
let main () =
  (* However, only warn if the value of the PATH environment cannot be
     gotten to set the default path, since the user may actually be
     setting another path with an upcoming -var.  *)
  (try
    set_path (Sys.getenv !path_var)
  with Not_found ->
    prerr_endline (!progname ^ ": warning: unable to get value of "
		   ^ !path_var ^ " for default path."));
  let argdefs = [
    ("--absolute", Arg.Clear relative_flag,
     "\tNon-absolute names are made absolute using the current diretory.");
    ("-A", Arg.Clear relative_flag,
     "\t\tNon-absolute names are made absolute using the current diretory.");
    ("--after", Arg.String set_add_after_mode,
     "item\tAdd next argument to the path after item");
    ("-a", Arg.String set_add_after_mode,
     "item\tAdd next argument to the path after item");
    ("--before", Arg.String set_add_before_mode,
     "item\tAdd the next argument to the path before item");
    ("-b", Arg.String set_add_before_mode,
     "item\tAdd the next argument to the path before item");
    ("--cmd", Arg.Unit (set_output Out_cmd),
     "\tOutput NT cmd.exe command to set path (not useful, alas)");
    ("--csh", Arg.Unit (set_output Out_csh),
     "\tOutput csh command to set the path");
    ("--current", Arg.Unit add_current,
     "\tAdd current directory to path");
    ("--delete", Arg.String delete,
     "item\tDelete all occurances of item in path");
    ("-d", Arg.String delete,
     "item\tDelete all occurances of item in path");
    ("--empty", Arg.Unit add_empty,
     "\tAdd an empty element to the path");
    ("-E", Arg.Unit add_empty,
     "\t\tAdd an empty element to the path");
    ("--end", Arg.Unit set_add_end_mode,
     "\tAdd next argument to the end of the path");
    ("-e", Arg.Unit set_add_end_mode,
     "\t\tAdd next argument to the end of the path");
    ("--exists", Arg.Set exists_flag,
     "item\tAdd next item only if it exists");
    ("--insep", Arg.String ((:=) in_path_sep),
     "sep\tSet the input path separator");
    ("--msys", Arg.Set msys_style,
     "\tOutput in msys style");
    ("--name", Arg.String ((:=) path_var),
     "name\tName of path for output");
    ("-n", Arg.String ((:=) path_var),
     "name\tName of path for output");
    ("--nice", Arg.Unit (set_output Out_nice),
     "\tPrint the path out \"nicely\", one item per line");
    ("--outsep", Arg.String ((:=) out_path_sep),
     "sep\tSet the output path separator");
    ("--path", Arg.String set_path,
     "path\tSet the path to work on (defaults to the value of PATH without\n"^
     "\t\t-path or -var)");
    ("-p", Arg.String set_path,
     "path\tSet the path to work on (defaults to the value of PATH without\n"^
     "\t\t-path or -var)");
    ("--quiet", Arg.Unit (set_output Out_quiet),
     "\tDon't print out the path");
    ("--relative", Arg.Set relative_flag,
     "\tInterpret non-absolute paths as relative to the current\n\t\tdirectory");
    ("-R", Arg.Set relative_flag,
     "\t\tInterprit non-absolute paths as relative to tthe current\n\t\tdirectory");
    ("--sep", Arg.String set_sep,
     "sep\tSet the input and output path separators");
    ("--sh", Arg.Unit (set_output Out_sh),
     "\t\tOutput sh command to set the path");
    ("--simple", Arg.Unit (set_output Out_simple),
     "\tOutput just the new value");
    ("--start", Arg.Unit set_add_start_mode,
     "\tAdd next argument to the start of the path");
    ("-s", Arg.Unit set_add_start_mode,
     "\t\tAdd next argument to the start of the path");
    ("--unique", Arg.Unit unique,
     "\tEliminate duplicate items");
    ("--var", Arg.String set_path_from_var,
     "var\tSet the path from the environment variable var");
    ("-v", Arg.String set_path_from_var,
     "var\tSet the path from the environment variable var");
    ("--warnings", Arg.Set warn_flag,
     ("\tWarn about missing environment variables instead of exiting \n"^
      "\t\twith an error"));
    ("-w", Arg.Set warn_flag,
     ("\t\tWarn about missing environment variables instead of exiting \n"^
      "\t\twith an error"));
    ("--version", Arg.Unit print_revision,
     "\tPrint version info and exit");
  ] in
  Arg.parse argdefs anonymous_arg
    ("\nusage: " ^ !progname ^ " [[options] [items] ...]\n"^
     "\nAdds items to the path, which defaults to the value of the PATH"^
     "\nenvironment variable, in positions specified by the user.\n"^
     "\nUse it with something like\n"^
     "\tfunction repath {\n"^
     "\t    eval $(modpath \"$@\" --unique)\n"^
     "\t}\nin your .bashrc or equivalent and always execute repath.\n"
    );
  begin match !mode with
  | Mode_after item ->
      prerr_endline (!progname ^ ": warning: no item to add -after " ^ item)
  | Mode_before item ->
      prerr_endline (!progname ^ ": warning: no item to add -before " ^ item)
  | Mode_start | Mode_end -> ()
  end;
  if !msys_style then begin
    out_path_sep := ":";
    path_list := msysize_path !path_list;
    if !output = Out_cmd then output := Out_sh;
  end;
  let final_path = "'" ^ (String.concat !out_path_sep !path_list) ^ "'" in 
  begin match !output with
  | Out_nice   -> List.iter print_endline !path_list
  | Out_simple -> print_endline final_path
  | Out_cmd    -> print_endline ("path " ^ final_path)
  | Out_csh    -> print_endline ("setenv " ^ !path_var ^ " " ^ final_path)
  | Out_sh     -> print_endline (!path_var ^ "=" ^ final_path ^ "\nexport "
				^ !path_var)
  | Out_quiet  -> ()
  end

let _ = main ()
