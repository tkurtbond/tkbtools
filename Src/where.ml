(* where.ml -- print out where in the path a file is found. *)

(* This will look see if the specified file exists a directory from
   the specified colon (or semi-colon on Windows) separated path.
   Unlike which, the path or environment variable to get the path from
   can be specified, and under Windows it can check if the file exists
   with common executable file extensions, such as .EXE.  *)

(* ??? Should this regularize the path under Cygwin?

   $ where -verbose -var SQLPATH ut_sp_ins_person.pkb -sep ";"
	   Trying c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Procedures;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Packages;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Triggers;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Views;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Test_Data;c/ut_sp_ins_person.pkb
	   Trying /svn/Compendium/DB/ORACLE/Utilities;c/ut_sp_ins_person.pkb
	   Trying /svn/CompendiumUnitTests/trunk;c/ut_sp_ins_person.pkb
	   Trying /home/tkb/job/ISR/DB/u;c/ut_sp_ins_person.pkb
	   Trying /home/tkb/job/ISR/DB/w/ut_sp_ins_person.pkb
*)


let revision_id = "$Id: where.ml 1.5 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let path_sep = ref (if Sys.os_type = "Win32" then ";" else ":")
let path_sep_rgx = ref (Str.regexp !path_sep)

let path_set = ref false
let path = ref []
let path_var = ref "PATH"

let found = ref false
let quiet_flag = ref false
let verbose_flag = ref false
let warn_flag = ref false

let san_rgx = (Str.regexp "^[ \t]*\"\\(.*\\)\"[ \t]*$")
let sanitize_element element =
  if Str.string_match san_rgx element 0 then
    Str.matched_group 1 element
  else
    element
  
let sanitize_path elements =
  match Sys.os_type with
  | "Win32" -> List.map sanitize_element elements
  | _ -> elements


let set_sep s =
  path_sep := s;
  path_sep_rgx := Str.regexp !path_sep

let set_path s =
  path := sanitize_path (Str.split !path_sep_rgx s);
  path_set := true

let set_path_from_var var =
  try set_path (Sys.getenv var)
  with Not_found ->
    prerr_endline ("where: " ^ (if !warn_flag then "warning" else "error")
		   ^ ": environment variable " ^ var ^ " is not set");
    if not !warn_flag then exit 3
    
(* PATHEXT tells Windows which suffixes should be considered as executables. *)
let suffixes = ref []
and suffixes_set = ref false

let add_suffix s = suffixes_set := true; suffixes := (s :: !suffixes)
let clear_suffixes () = suffixes := []

let set_default_suffixes () =
  suffixes := 
    match Sys.os_type with
    | "Win32" -> begin
	try (Str.split !path_sep_rgx (Sys.getenv "PATHEXT"))
	with Not_found -> [".bat"; ".exe"; ".com"; ".dll"]
      end
    | _ -> []


let file_exists cmd dir =
  let check_file filename =
    if !verbose_flag then begin
      prerr_endline ("\tTrying " ^ filename)
    end;
    if Sys.file_exists filename then begin
      found := true;
      if not !quiet_flag then print_endline filename
    end in     
  let filename = Filename.concat dir cmd in
  check_file filename;
  List.iter
    (fun suffix ->
       (* print_endline ("Suffix: " ^ suffix); *)
       let filename = Filename.concat dir (cmd ^ suffix) in
       check_file filename)
    !suffixes
    
let process_cmd cmd =
  if not !path_set then set_path_from_var "PATH";
  if not !suffixes_set then set_default_suffixes ();
  (* print_endline "path:";
  List.iter print_endline !path;
  print_endline "suffixes:";
  List.iter print_endline !suffixes; *)
  List.iter (file_exists cmd) !path

let main () = 
  let argdefs =
    [("-addsuffix", Arg.String add_suffix,
      " suffix\tAdd suffix to list of search suffixes");
     ("-clearsuffix", Arg.Unit clear_suffixes,
      "\tClear the list of search suffixes");
     ("-path", Arg.String set_path,
      " path\tPath to search along");
     ("-quiet", Arg.Set quiet_flag,
      "\tProduce no output; successful exit indicates command was found");
     ("-sep", Arg.String set_sep,
      " sep\tSet the path separator");
     ("-var", Arg.String set_path_from_var,
      " pathvar\tName of environment variable containing path");
     ("-verbose", Arg.Set verbose_flag,
      "\tPrint each filenamed checked");
     ("-version", Arg.Unit print_revision,
      "\tPrint version info and exit");
     ("-warn", Arg.Set warn_flag,
      "\tWarn on undefined environment variables instead of exiting");
    ] in
  Arg.parse argdefs process_cmd "usage: where [options] command [...]";
  exit (if !found then 0 else 3)

let _ = 
  Printexc.catch main ()

print_endline "Endit"
