let verbose = ref false

let matches = ref []
let add_match m = matches := m @ !matches

let process matches =
  (* List.iter process_match *) ()

let main () =
  let specs = [
    ("-v", Arg.Set verbose, "verbose messages")
  ] in
  Arg.parse specs add_match "usage: matchname filenameregexp [...]";
  process (List.rev !matches)
