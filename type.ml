type lisp_code =
    Quote of lisp_expr
  | Var of int * int
  | Global of symbol
  | If of lisp_code * lisp_code * lisp_code
  | Application of lisp_code * (lisp_code array)
  | Build_closure of lisp_code
  | Global_set of symbol * lisp_code
  | Global_set_macro of symbol * lisp_code
and lisp_expr =
    Nil
  | Int of int
  | Symbol of symbol
  | Cons of lisp_expr * lisp_expr
  | Builtin of (lisp_expr array -> lisp_expr)
  | Func of string list * lisp_expr
  | Closure of lisp_code * (lisp_expr array list)
  | Compiled_closure of (lisp_expr array list -> lisp_expr) * (lisp_expr array list)
and symbol_value =
    Value of lisp_expr
  | Macro of lisp_expr
and symbol = { name: string ; mutable value: symbol_value };;

let heusl x = x;;
