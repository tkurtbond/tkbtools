exception Error of string

type op_fun = string * (string -> string)
type op_list = op_fun list

val add_ops : op_list -> unit
val get_ops : unit -> op_list
