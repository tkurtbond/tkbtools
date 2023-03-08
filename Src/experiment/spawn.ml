(* spawn.ml -- Spawn a process when we don't have access to a shell.  *)

(* Note: I had hoped this would be  useful in Emacs's dired with command `!',
   but it isn't.  Emacs waits for all the children to die before returning.
   Maybe we could do something with process groups?  :(  *)

(* Todo:
   * Options to set stdin, stdout, and stderr?
*)

let main () =
  if (Array.length Sys.argv) < 2 then begin
    prerr_endline "usage: spawn command [args]";
    exit 2
  end;
  let prog = Sys.argv.(1) in
  let args = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
  let child_pid = Unix.fork () in
  if child_pid = 0 then begin
    prerr_endline "First child";
    let child_pid = Unix.fork () in
    if child_pid = 0 then begin
      prerr_endline "Second child";
      let pid =
	Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr in
      prerr_endline "Second child, after create_process";
      exit 0;
    end;
    prerr_endline "First child, exiting";
    exit 0
  end;
  prerr_endline "main program, exiting";
  exit 0

let _ = Printexc.print main ()
