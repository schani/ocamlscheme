(*
 * symtab.ml
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
