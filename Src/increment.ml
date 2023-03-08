(* increment.ml -- find the first file name in a series that doesn't exist. *)
open Unix
open Printf

let revision_id =
  "$Id: increment.ml 1.3 Mon, 15 Feb 1999 22:39:46 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

exception No_commands of string

let set_int argname nref s =
  try let n = int_of_string s in nref := n
  with _ ->
    eprintf "increment: invalid %s argument: %s\n" argname s;
    exit 2

let width = ref 0
and start = ref 0
and prefix_date = ref false
and suffix_date = ref false

let args = ref ([] : string list)

let left_pad padding n s =
  let l = String.length s in
  if l >= n then
    s
  else begin
    (String.make (n - l) padding) ^ s
  end

let date, time = 
  let time = Unix.localtime (Unix.time ()) in
  let date = (sprintf "%04d-%02d-%02d"
		(time.Unix.tm_year + 1900)
		(time.Unix.tm_mon + 1)
		time.Unix.tm_mday)
  and time = (sprintf "%02d-%02d-%02d"
		time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec)
  in date, time

let print_usage () =
  List.iter eprintf
    [ "\n";
      "usage: increment [-s n | -version | -w n] [--] filename-components\n\n";
      "\tGenerate a filename, substituting in date, time, and the next\n";
      "\tnumber in the sequence\n\n";
      "Example:\n";
      "\tincrement backup- d - t _ n .tar.gz\n";
      "\tprints \"backup-2003-04-25-13-27-08_0.tar.gz\" if that file does\n";
      "\tnot exist, or \"backup-2003-04-25-13-27-08_1.tar.gz\" if it does\n";
      "\n";
      "options are:\n";
      "  -s n\tNumber to start with\n";
      "  -version\tPrint version and exit\n";
      "  -w n\tWidth to pad number to\n";
      "filename-components are:\n";
      "  d\tInsert the date in the form YYYY-MM-DD\n";
      "  n\tInsert the next number in the sequence\n";
      "  t\tInsert the time in the form HH-MM-SS\n"; 
      "  <x>\tAnything else is inserted literally\n\n"; ]; 
  exit 3

let preprocess_args args =
  let rec iter args acc =
    match args with
    | [] -> (List.rev acc)
    | ("-help" | "--help") :: rest -> print_usage () (* exits *)
    | "-version" :: rest -> print_revision () (* exits *)
    | "-s" :: num :: rest -> set_int "-s" start num; iter rest acc
    | "-w" :: num :: rest -> set_int "-w" width num; iter rest acc
    | "--" :: rest -> (List.rev acc) @ rest
    | x :: rest ->
	if (String.length x) >= 1 && x.[0] = '-' then begin 
	  eprintf "increment: unknown option: %s\n" x;
	  print_usage ()		(* exits *)
	end else (List.rev acc) @ args
  in iter !args []

let anon_args s = args := !args @ [s]

let main () =
(* Why aren't I using this?
  let argdefs = [
    ("-w", Arg.Int (set_int width), "width\tWidth of number");
    ("-s", Arg.Int (set_int start), "start\tNumber to start");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
    ("--", Arg.Rest anon_args, "end options");
  ]
  and usage = "usage: increment [options] prefix [suffix]" in
  Arg.parse [] anon_args usage;
  (* Make sure under WNT that we don't end up with CR/LF at the end. *)
*)
  let argv = Array.init ((Array.length Sys.argv) - 1)
      (function i -> Sys.argv.(i + 1))
  in Array.iter anon_args argv; 
  args := preprocess_args args;
  let make_name n =
    (* O'caml sprintf doesn't allow "%0*d", alas.  *)
    let rec f cmds acc ncmd =
      match cmds with
      | [] -> (String.concat "" (List.rev acc)), ncmd
      |	"--" :: x :: rest -> f rest (x :: acc) ncmd
      | "d" :: rest -> f rest (date :: acc) (ncmd + 1)
      |	"t" :: rest -> f rest (time :: acc) (ncmd + 1)
      | "n" :: rest ->
	  f rest ((left_pad '0' !width (string_of_int n))  :: acc) (ncmd + 1)
      | x :: rest -> f rest (x :: acc) ncmd
    in f !args [] 0
  in
  let n = ref !start and found = ref false in
  while not !found do
    let (name, ncmd) = make_name !n in
    (if ncmd < 1 then
      let args = (String.concat " " !args) in
      eprintf "increment: no commands found in %s so nothing increments!\n"  name;
      raise (No_commands args));
    try
      ignore (stat name);
      incr n
    with
    | Unix_error (ENOENT, _, _) ->
      found := true;
      print_endline name
    | x -> prerr_endline "Unexpected error: ";
	found := true;
	raise x
  done


let _ =
  Printexc.catch main ()
