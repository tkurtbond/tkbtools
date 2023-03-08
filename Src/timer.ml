(* timer.ml -- a simple non-graphical timer. *)

let revision_id = "$Id: timer.ml 1.5 Sat, 10 Feb 2001 21:10:15 -0500 tkb $"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0


open Unix

let time_delta = ref 5.0
let dr = Str.regexp "\\([0-9]+\\(\\.[0-9]+\\)?\\)\\([HhMmSs]?\\)"
let set_delta s =
  let rec iter d pos =
    if Str.string_match dr s pos then begin
      let d' = float_of_string (Str.matched_group 1 s) in
      let scale =
	match Str.matched_group 3 s with
	| "" | "S" | "s" -> 1.
	| "H" | "h" -> 3600.
	| "M" | "m" -> 60.
	| _ -> 1.			(* This can't actually happen. *)
      in iter (d +. (d' *. scale)) (Str.group_end 0)
    end else
      time_delta := d
  in iter 0. 0
  


let interval = ref 5.0
let set_interval i = interval := i

let num_beeps = ref 5
and between_beeps = ref 2

let save_int ir i = ir := i

let handle_sig n =
  time_delta := !time_delta -. !interval;
  if !time_delta <= 0.0 then begin
    print_string "Time is up!";
    flush Stdlib.stdout;
    print_char '\007'; flush Stdlib.stdout;
    for i = 2 to !num_beeps do
      sleep !between_beeps;
      print_char '\007'; flush Stdlib.stdout;
    done;
    print_newline ();
    exit 0;
  end else begin
    print_endline ("Time remaining: " ^ (string_of_float !time_delta) ^
		   " seconds");
    flush Stdlib.stdout;
  end

let main () =
  let specs = [
    ("-interval", Arg.Float set_interval,
     "seconds\tThe interval between updates (a float)");
    ("-beeps", Arg.Int (save_int num_beeps),
     "numbeeps\tThe number of times to beep");
    ("-between", Arg.Int (save_int between_beeps),
     "seconds\tThe number of seconds to sleep between beeps");
    ("-version", Arg.Unit print_revision, "Print version info and exit");
  ]
  and usage = "usage: timer [options] time-delta" in
  Arg.parse specs set_delta usage;
  (* Signals are in module Sys, but itimers are in module Unix. *)
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handle_sig));
  ignore (setitimer ITIMER_REAL { it_interval = !interval; 
				  it_value = !interval }); 
  while true do
    pause ();
  done

let _ =
  Printexc.catch main ()
