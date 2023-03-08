(* nxt.ml -- Start a numbered xterm.  obsolete - 2019-10-24 *)

let revision_id = "$Id: nxt.ml 1.7 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


let progname = ref "nxt"

let init_flag = ref false	(* If initializing, don't start a term. *)
and info_flag = ref false	(* Print info about lock file and exit. *)
and label_flag = ref false      (* Use a different label. *)
and label_value = ref ""        (* The alternate label specified. *)
and start_value = ref 0		(* Start numbering terms at this value. *)
and term_flag = ref false	(* An alternate terminal was specified. *)
and term_value = ref "xterm"	(* The alternate terminal specified. *)
and args = ref []		(* Collected arguments to pass to terminal. *)

let set_label s =
  label_flag := true;
  label_value := s

let set_start n =
  start_value := n - 1		(* Subtract 1, since we always add 1 below. *)

let set_term s =
  term_flag := true;
  term_value := s

let anon_arg s =
  args := s :: !args

let get_lock_file () =
  let lock_dir = 
    try Sys.getenv "HOME" with Not_found ->
      try
	(Unix.getpwnam (try Sys.getenv "LOGNAME" with Not_found ->
	                try Sys.getenv "USER" with Not_found ->
		          Unix.getlogin ())).Unix.pw_dir
      with Not_found ->
	prerr_endline (!progname ^ ": error: unable to find lock directory");
	exit 3
  and lock_display =
    try Sys.getenv "DISPLAY" with Not_found ->
      prerr_endline (!progname ^
		     ": error: DISPLAY environment variable undefined");
      exit 5
  in (Filename.concat lock_dir (".nxt-lock" (* ^ "-" ^ lock_display *) ))
    (* Under linux, in .xsession DISPLAY is :0, but later is :0.0.  Why??? *)


let get_next lock_file =
  let fd = Unix.openfile lock_file
             [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
  Unix.lockf fd Unix.F_LOCK 0;
  let line =
    try input_line (Unix.in_channel_of_descr fd)
    with End_of_file -> ""
  in let items = Str.bounded_split (Str.regexp "[ \t]+") line 3 in
  let (n, term, label) =
    match items with
      [] -> (!start_value, !term_value, !label_value)
    | [n] -> (int_of_string n, !term_value, !label_value)
    | [n; term] -> (int_of_string n, term, !label_value)
    | [n; term; label] -> (int_of_string n, term, label)
    | _ -> failwith "Impossible!  Bounded split became unbounded!"
  in let n = n + 1 in
  let s = string_of_int n in 
  let oline =  s ^ " " ^ term ^
    (if label  = "" then "" else  " " ^ label) ^ "\n" in
  (* FIXME: I really ought to check the return values for sanity... *)
  ignore (Unix.lseek fd 0 Unix.SEEK_SET);
  ignore (Unix.write fd oline 0 (String.length oline));
  Unix.lockf fd Unix.F_ULOCK 0;
  Unix.close fd;
  (s, term, label)	(* The number of this term, and the term.  *)


let init_counter filename term label =
  let outf = open_out filename in
  output_string outf ((string_of_int !start_value) ^ " " ^ term ^ (if label  = "" then "" else  " " ^ label) ^ "\n");
  close_out outf

let main () =
  (* Hmm.  In order to be able to pass options to the program we are
     starting, we used to use a modified version of the Arg module.
     However, equivalent functionality was added in Ocaml 1.08.  *)
  let argdefs = [
    ("-info", Arg.Set info_flag, "\tDisplay info");
    ("-init", Arg.Set init_flag, "\tInitialize the counter");
    ("-label", Arg.String set_label, "label\tUse label instead of term name");
    ("-start", Arg.Int set_start, "n\tStart numbering at n");
    ("-term", Arg.String set_term, "\tSpecify the terminal program to use");
    ("--", Arg.Rest anon_arg, "\t\tTreat other options as arguments");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ] in
  Arg.parse argdefs anon_arg ("usage: " ^ !progname
			      ^ " [options] [arguments]\n");
  let lock_file = get_lock_file () in
  if !info_flag then begin
    prerr_endline (!progname ^ ": lock file: " ^ lock_file);
    exit 0
  end;
  if !init_flag then begin
    init_counter lock_file !term_value !label_value;
    exit 0
  end;
  let (s, term, label) = get_next lock_file in
  let term = if !term_flag then !term_value else term in
  let label =
    if !label_flag then
      !label_value
    else if label = "" then term
    else label in 
  Unix.execvp term (Array.of_list 
		      ([term; "-title"; label ^ "-" ^ s; "-name"; label ^ "-" ^ s]
		       @ (List.rev !args)))
  

let _ = 
  Printexc.catch main ()
