(* Blast it. truncate is not implemented under Windows. *)

let size = ref 0

let whack_file filename =
  Unix.truncate filename !size

let _ =
  let specs = [("-s", Arg.Int ((:=) size), "new size of file")] in
  Arg.parse specs whack_file "usage: truncfile [-s NUMBER] file ..."
