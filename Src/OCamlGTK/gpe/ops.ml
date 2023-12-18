exception Error of string

type op_fun = string * (string -> string)
type op_list = op_fun list

let ops = ref []

let add_ops new_ops = ops := !ops @ new_ops; ()

let get_ops () = !ops


(* let subst c c' s = 
  for i = 0 to (Bytes.length s) - 1 do
    if s.[i] = c then Bytes.set s i c'
  done

let convert c c' s =
  let t =  String.copy s in 
  subst c c' t;
  t
 *)
let convert c c' s = 
  String.map (fun x -> if x = c then c' else x) s



let bslash = convert '/' '\\'
and fslash = convert '\\' '/'


let _ =
  add_ops [("\\", bslash)
	  ;("/",  fslash)
	  ;("Lower", String.lowercase_ascii)
	  ;("Upper", String.uppercase_ascii)]
	     
