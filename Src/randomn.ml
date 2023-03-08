let print_version () = prerr_endline "version 0.1"; exit 0

let x = ref 0
let n = ref 1

let process_int s =
  let limit = int_of_string s in
  for i = 1 to !n do 
    Printf.printf "%d\n" ((Random.int limit) + !x)
  done


let main () =
  Random.self_init ();
  let specs = [("-+", Arg.Int ((:=) x),
		"x\tAdd x to random number;\n"^
		"\t-+ 1 for 1-based (0-based is default)");
	       ("-n", Arg.Int ((:=) n),
		"n\tOutput n random numbers");
	       ("-version", Arg.Unit print_version,
		"\tPrint version and exit")]
  and usage = "usage: random n [...]" in
  Arg.parse specs process_int usage

let _ = Printexc.print main ()
