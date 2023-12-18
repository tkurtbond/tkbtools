(* mingw_sysops.ml -- Add Windows specific operations. *)

exception Short_name_error of string

let _ =
  Callback.register_exception "short name exception"
    (Short_name_error "any string")


external to_short_name: string -> string = "to_short_name"

let short_error s msg =
  ("Unexpected error converting path element \n\""
   ^ s ^ "\"\n to short form: " ^ msg)

let short_op path_element =
  try to_short_name path_element with
    Short_name_error msg -> raise (Ops.Error msg)

let _ = Ops.add_ops [("Short", short_op)]  
