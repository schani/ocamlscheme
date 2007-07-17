(*
 * type.ml
 *
 * ocamlscheme
 *
 * Copyright (C) 2001-2004 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

type lisp_code =
    Quote of lisp_expr
  | Var of int * int
  | Global of symbol
  | If of lisp_code * lisp_code * lisp_code
  | Application of lisp_code * (lisp_code array)
  | Build_closure of lisp_code
  | Amb of lisp_code list
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
