(* words.mll -- print out the words or identifiers in a file. *)
{
exception Eof 
} 

rule word = parse
  ['A'-'Z' 'a'-'z']+	{ Lexing.lexeme lexbuf }
| _ 			{ word lexbuf }
| eof 			{ raise Eof }
and identifier = parse
  ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
			{ Lexing.lexeme lexbuf }
| _                     { identifier lexbuf }
| eof 			{ raise Eof }
and vms_basic = parse
  ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_'  '$' '.' '%' '0'-'9']*
			{ Lexing.lexeme lexbuf }
| _			{ vms_basic lexbuf }
| eof 			{ raise Eof }


{
open Printf

let minimum_length = ref 1
and verbose_flag = ref false

let files_processed = ref 0

let tokenizer = ref word

let use_identifier () = tokenizer := identifier

let use_vms_basic () =  tokenizer := vms_basic


let process_chnl chnl =
  try 
    let lb = Lexing.from_channel chnl in
    while true do
      let s = !tokenizer lb in 
      if (String.length s) >= !minimum_length then print_endline s
    done
  with Eof -> ()


let process_filename filename =
  (* If there is even one file named on the command line we don't act as a
     filter, even if we can't process that line. *)
  incr files_processed;
  try 
    let chnl = open_in filename in
    process_chnl chnl
  with Sys_error msg ->
    eprintf "unable to open %s: %s\n" filename msg
    
  
      

let main () =
  let argdefs =
    [("-b", Arg.Unit use_vms_basic, "Output VMS BASIC identifiers, not words");
     ("-i", Arg.Unit use_identifier, "Output identifiers, not words");
     ("-n", Arg.Int ((:=) minimum_length),
      "len\tminimum length of words to report");
     ("-v", Arg.Set verbose_flag, "verbose reporting")]
  and usage_msg = "usage: words [options] [file ...]" in
  Arg.parse argdefs process_filename usage_msg;
  if !files_processed = 0 then
    process_chnl stdin;
  if !verbose_flag then
    eprintf "files processed: %d\n" !files_processed


let _ = Printexc.catch main ()
} 

