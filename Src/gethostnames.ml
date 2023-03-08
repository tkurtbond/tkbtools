(* gethostnames.ml -- get the host names from ip addresses*)

(* 2006/09/05 - Should we get using getaddrinfo(3)? *)

let revision_id = "0.1"
let print_revision () = Printf.printf "%s\n" revision_id; exit 0

let num_files    = ref 0
let num_args     = ref 0
let verbose_flag = ref false




let print_host_entry host =
  let address_list =
    Array.fold_right
      (fun addr acc ->
	(Unix.string_of_inet_addr addr) ^
	(if String.length acc = 0 then "" else " ") ^ acc)
      host.Unix.h_addr_list "" in
  Printf.printf "%15s: %s\n" address_list host.Unix.h_name


let process_address arg =
  begin try
    if !verbose_flag then prerr_endline ("Parsing inet addr out of " ^ arg);
    let addr = Unix.inet_addr_of_string arg in
    if !verbose_flag then
      prerr_endline ("Getting host name by addr for " ^ arg);
    let host = Unix.gethostbyaddr addr in print_host_entry host
  with Not_found ->
    Printf.printf "%15s: Non-existent host/domain\n" arg
  end;
  flush stdout

let process_ip_arg arg =
  incr num_args;
  process_address arg


let process_hostname arg = 
  begin try
    if !verbose_flag then begin
      prerr_endline ("Getting host ip address by name for " ^ arg);
    end;
    let host = Unix.gethostbyname arg in print_host_entry host
  with Not_found ->
    Printf.printf "%15s: *** Non-existent host/domain\n" arg
  end;
  flush stdout


let process_name_arg arg =
  incr num_args;
  process_hostname arg
  

let process_channel chnl = 
  try while true do
    let line = input_line chnl in
    try 
      process_address line
    with
    | Failure (_) -> process_hostname line
  done with End_of_file -> ()


let process_file filename =
  let in_chnl = open_in filename in
  incr num_files;
  process_channel in_chnl;
  close_in in_chnl


let main () =
  let args =
    [ ("-ip",      Arg.String process_ip_arg,
       "address, where address is a dotted numeric IP address");
      ("-name",    Arg.String process_name_arg,
       "name, where name is a hostname");
      ("-verbose", Arg.Set verbose_flag,
       "print verbose messages");
      ("-version", Arg.Unit print_revision,
       "print version");
    ]
  and usage =
    "usage: gethostnames [-ip addr [-ip addr]... | <inputfile ] " ^
    "[filename ...]\n" ^
    "       The input should contain one IP address in dotted numeric form\n" ^
    "       per line" in
  Arg.parse args process_file usage;
  if (!num_files < 1) && (!num_args < 1) then
    process_channel stdin


let _ =
  Printexc.print main ()

