open Type
open List

let global_env = ref [];;

let global_lookup name =
  find (function x -> match x with { name = n } -> n = name) !global_env;;

let global_set name value =
  try
    match find (function x -> match x with { name = n } -> n = name) !global_env with
      x -> x.value <- value ; x
  with Not_found ->
    let symbol = { name = name ; value = value }
    in global_env := symbol :: !global_env ;
    symbol;;

let intern name =
  try
    global_lookup name
  with Not_found ->
    global_set name (Value Nil);;
