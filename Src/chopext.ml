module F = Filename

let main () = 
  let filename = Sys.argv.(1) in
  let ext = Sys.argv.(2) in
  if (Filename.check_suffix filename ext) then
    print_endline (Filename.chop_suffix filename ext)
  else
    print_endline filename

let _ = Printexc.print main ()
