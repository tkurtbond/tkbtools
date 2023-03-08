(* unixtime.ml - convert unix time in seconds w/fraction into human date. *)
open Unix
open Arg

let print_microseconds = ref false
let to_time = ref localtime

let chop msecs =
  assert (msecs < 1.0);
  let s = Printf.sprintf "%0f" msecs in 
  String.sub s 1 ((String.length s) - 1)

let decode_time seconds =
  let tm = !to_time seconds in
  let (seconds', tm') = Unix.mktime tm in
  let msecs = seconds -. seconds' in 
  Printf.sprintf
    "%04d/%02d/%02d %02d:%02d:%02d%s"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
    (if !print_microseconds then chop msecs else "")
    

let display_time seconds =
  Printf.printf "%f -> %s\n" seconds (decode_time seconds)

let display_arg secstr =
  let seconds = float_of_string secstr in display_time seconds

let num_args = ref 0

let do_arg secstr = incr num_args; display_arg secstr

let main () =
  let argdefs =
    [ ("-u", Unit (fun () -> to_time := gmtime), "Display using UTC.")
    ; ("-l", Unit (fun () -> to_time := localtime), "Display using localtime.")
    ; ("-t", Float display_time, "Display a particular time")
    ; ("-m", Set print_microseconds, "Print microseconds")
    ] in
  parse argdefs do_arg "usage: unixtime [-t time] [time]";
  if !num_args = 0 then
    (* There has to be a better way. *)
    let rec loop () =
      let continue =
	try 
	  let line = read_line () in
	  begin try 
	    display_arg line
	  with _ -> ()
	  end;
	  true
	with End_of_file -> false
      in if continue then loop () else ()
    in loop ()

      
let _ = Printexc.print main ()
