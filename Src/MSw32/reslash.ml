let windows_separator_flag = ref true

let process_filename filename =
  let s = String.copy filename in
  for i = 0 to (String.length filename) - 1 do
    if !windows_separator_flag then begin
      if s.[i] = '/' then s.[i] <- '\\'
    end else begin
      if s.[i] = '\\' then s.[i] <- '/'
    end
  done;
  print_endline s

let main () =
  let argdefs = [
    ("-w", Arg.Set windows_separator_flag,
     "\tOutput using MS WINDOWS filename separators: \\ (default)");
    ("-u", Arg.Clear windows_separator_flag,
     "\tOutput using Unix filename separators: /");
  ] in
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-w|-u] filename ..." in
  Arg.parse argdefs process_filename usage

let _ =
  Printexc.catch main ()
