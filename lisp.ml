(*
 * lisp.ml
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
open Symtab
open Format
open List
exception Hell
exception Not_a_list
exception Not_a_symbol
exception Not_a_value_symbol
exception Not_an_int
exception Invalid_args
exception Not_a_function of lisp_expr
exception Amb_not_supported

let rec position f l =
  match l with
    [] -> raise Not_found
  | x :: r -> if f x then 0 else 1 + position f r;;

(*
let rec prindebug x =
  match x with
    Nil -> print_string "nil ()"
  | Int i -> printf "int %i" i
  | Symbol { name = n } -> printf "symbol %s" n
  | Cons(car, cdr) -> print_string "cons ("; prindebug car; print_string " . "; prindebug cdr; print_string ")"
  | _ -> raise Hell;;
*)

let rec prinlist x =
  match x with
    Cons(car, Cons(cadr, cddr)) -> prin1 car; print_string " "; prinlist(Cons(cadr, cddr))
  | Cons(car, Nil) -> prin1 car
  | Cons(car, cdr) -> prin1 car; print_string " . "; prin1 cdr
  | _ -> raise Not_a_list
and prin1 x =
  match x with
    Nil -> print_string "()"
  | Int i -> print_int i
  | Symbol { name = n } -> print_string n
  | Cons(car, cdr) -> print_string "("; prinlist(Cons(car, cdr)); print_string ")"
  | Builtin _ -> print_string "#<builtin>"
  | Func _ -> print_string "#<func>"
  | Closure _ -> print_string "#<closure>"
  | Compiled_closure _ -> print_string "#<compiled_closure>";;

let rec uncons_list l =
  match l with
      Nil -> []
    | Cons (x, cdr) -> x :: uncons_list cdr
    | _ -> raise Not_a_list;;

let rec uncons_symbols x =
  List.map (function Symbol s -> s
	      | _ -> raise Not_a_symbol)
    (uncons_list x)

(*
let rec apply f a e =
  match f with
    Builtin b -> b (Array.of_list a)
  | Func(names, body) -> eval body (append (map2 (function x -> function y -> (x,y)) names a) e)
  | _ -> raise Hell
and eval x e =
  match x with
    Nil -> x
  | Int _ -> x
  | Symbol s -> assoc s e
  | Cons(Symbol "quote", Cons(x, Nil)) -> x
  | Cons(Symbol "if", Cons(cond, Cons(cons, Cons(alt, Nil)))) -> if eval cond e == Nil then eval alt e else eval cons e
  | Cons(Symbol "lambda", Cons(args, Cons(body, Nil))) -> Func(uncons_symbols args, body)
  | Cons(Symbol "define", Cons(Symbol name, Cons(value, Nil))) -> global_env := (name, ref (eval value e)) :: !global_env; Nil
  | Cons(func, args) -> apply (eval func e) (eval_args args e) e
  | Builtin _ -> x
  | Func _ -> x
  | Closure _ -> x
and eval_args a e =
  match a with
    Nil -> []
  | Cons(arg, rest) -> eval arg e :: eval_args rest e
  | _ -> raise Hell;;
*)

let nil_symbol = intern "nil"
and t_symbol = intern "t";;

let symbol_value_value s =
  match s.value with
    Value v -> v
  | _ -> raise Not_a_value_symbol;;

let builtin_plus args =
  Array.fold_left (function x -> function y -> match (x, y) with (Int(i), Int(j)) -> Int(i + j) | _ -> raise Not_an_int) (Int(0)) args;;
let rec builtin_minus args =
  match args.(0) with
    Int(i) ->
      (
       match builtin_plus(args) with
	 Int(j) -> Int(i + i - j)
       | _ -> raise Not_an_int
      )
  | _ -> raise Not_an_int;;
let rec builtin_mult args =
  Array.fold_left (function x -> function y -> match (x, y) with (Int(i), Int(j)) -> Int(i * j) | _ -> raise Not_an_int) (Int(1)) args;;
let builtin_numbers_equal args =
  match args with
    [|Int(i); Int(j)|] -> if i = j then Symbol t_symbol else Nil
  | _ -> raise Invalid_args;;
let builtin_less args =
  match args with
    [|Int(i); Int(j)|] -> if i < j then Symbol t_symbol else Nil
  | _ -> raise Invalid_args;;
let builtin_cons args =
  match args with
    [|car ; cdr|] -> Cons (car, cdr)
  | _ -> raise Invalid_args;;
let builtin_car args =
  match args with
    [|Cons (car, _)|] -> car
  | _ -> raise Invalid_args;;
let builtin_cdr args =
  match args with
    [|Cons (_, cdr)|] -> cdr
  | _ -> raise Invalid_args;;
let builtin_nullp args =
  match args with
      [|arg|] -> if arg = Nil then Symbol t_symbol else Nil
    | _ -> raise Invalid_args;;
let builtin_begin args =
  args.((Array.length args) - 1)

let rec env_lookup name e =
  match e with
    [] -> raise Not_found
  | a :: r ->
      try
	(0, position (function x -> x = name) a)
      with Not_found ->
	match env_lookup name r with
	  (depth, index) -> (depth + 1, index);;

let rec compile x e =
  match x with
    Nil -> Quote x
  | Int _ -> Quote x
  | Builtin _ -> Quote x
  | Func _ -> Quote x
  | Closure _ -> Quote x
  | Compiled_closure _ -> Quote x
  | Symbol s ->
      (
       try
	 match env_lookup s e with
	   (depth, index) -> Var (depth, index)
       with Not_found ->
	 Global s
      )
  | Cons(Symbol { name = "quote" }, Cons(x, Nil)) -> Quote(x)
  | Cons(Symbol { name = "if" }, Cons(cond, Cons(cons, Cons(alt, Nil)))) ->
      If (compile cond e, compile cons e, compile alt e)
  | Cons(Symbol { name = "lambda" }, Cons(args, Cons(body, Nil))) ->
      Build_closure (compile body ((uncons_symbols args) :: e))
  | Cons(Symbol { name = "amb" }, args) ->
      Amb (List.map (fun x -> compile x e) (uncons_list args))
  | Cons(Symbol { name = "define" }, Cons(Symbol { name = name }, Cons(value, Nil))) ->
      Global_set (global_lookup name, compile value e)
  | Cons(Symbol { name = "define-macro" }, Cons(Symbol { name = name }, Cons(value, Nil))) ->
      Global_set_macro (global_lookup name, compile value e)
  | Cons(func, args) ->
      Application (compile func e, Array.of_list (compile_args args e))
and compile_args a e =
  match a with
    Nil -> []
  | Cons(arg, rest) -> compile arg e :: compile_args rest e
  | _ -> raise Not_a_list;;

let rec bcapply f a =
  match f with
    Builtin b -> b a
  | Closure (c, e) -> bceval c (a :: e)
  | _ -> raise (Not_a_function f)
and bceval x e =
  match x with
    Quote q -> q
  | Global s -> symbol_value_value s
  | If (cond, cons, alt) -> if bceval cond e == Nil then bceval alt e else bceval cons e
  | Application (f, args) -> bcapply (bceval f e) (Array.map (function a -> bceval a e) args)
  | Build_closure c -> Closure (c, e)
  | Amb _ -> raise Amb_not_supported
  | Global_set (s, c) -> s.value <- Value (bceval c e) ; Nil
  | Global_set_macro (s, c) -> s.value <- Macro (bceval c e) ; Nil
  | Var (depth, index) -> (nth e depth).(index);;

let rec csbcapply f a next cont =
  match f with
      Builtin b -> cont (b a) next
    | Closure (c, e) -> csbceval c (a :: e) next cont
    | _ -> raise (Not_a_function f)
and csbceval x e next cont =
  let rec eval_args args evaled_args index next cont =
    if index = (Array.length args) then
      cont evaled_args next
    else
      csbceval args.(index) e next
	(fun arg next ->
	   Array.set evaled_args index arg;
           eval_args args evaled_args (index + 1) next cont)
  and eval_amb args =
    match args with
	[] -> next ()
      | arg :: rest ->
	  csbceval arg e
	    (fun dummy ->
	       eval_amb rest)
	    cont
  in
    match x with
	Quote q -> cont q next
      | Global s -> cont (symbol_value_value s) next
      | If (cond, cons, alt) ->
	  csbceval cond e next
	    (fun cond next ->
	       if cond == Nil then
		 csbceval alt e next cont
	       else
		 csbceval cons e next cont)
      | Application (f, args) ->
	  let evaled_args = Array.make (Array.length args) Nil
	  in eval_args args evaled_args 0 next
	       (fun args next ->
		  csbceval f e next
		    (fun f next ->
		       csbcapply f args next cont))
      | Build_closure c -> cont (Closure (c, e)) next
      | Amb args ->
	  eval_amb args
      | Global_set (s, c) ->
	  csbceval c e next
	    (fun v next ->
	       s.value <- Value v;
	       cont Nil next)
      | Global_set_macro (s, c) ->
	  csbceval c e next
	    (fun v next ->
	       s.value <- Macro v;
	       cont Nil next)
      | Var (depth, index) -> cont (nth e depth).(index) next;;

let rec macroexpand_list x =
  match x with
    Nil -> []
  | Cons (car, cdr) -> macroexpand car :: macroexpand_list cdr
  | _ -> raise Not_a_list
and macroexpand x =
  match x with
    Cons (Symbol { value = Macro m }, args) -> (bcapply m (Array.of_list (macroexpand_list args)))
  | Cons (car, cdr) -> Cons (macroexpand car, macroexpand cdr)
  | _ -> x;;

(*
let rec bccompile x =
  match x with
    Quote q -> (function e -> q)
  | Global r -> (function e -> !r)
  | If (cond, cons, alt) ->
      let condc = bccompile cond
      and consc = bccompile cons
      and altc = bccompile alt
      in (function e -> if condc e == Nil then altc e else consc e)
  | Application (f, args) ->
      let fc = bccompile f
      and argsc = Array.map bccompile args
      in (function e ->
	let f = fc e
	and args = Array.map (function a -> a e) argsc
	in match f with
	  Builtin b -> b args
	| Compiled_closure (c, ce) -> c (args :: ce)
	| _ -> raise Hell)
  | Build_closure c -> 
      let cc = bccompile c
      in (function e -> Compiled_closure (cc, e))
  | Global_set (s, v) ->
      let vc = bccompile v
      in (function e -> s := vc e; Nil)
  | Var (depth, index) -> (function e -> (nth e depth).(index));;
*)

let main () =
  t_symbol.value <- Value (Symbol t_symbol) ;
  nil_symbol.value <- Value (Nil) ;
  (intern "+").value <- Value (Builtin builtin_plus) ;
  (intern "-").value <- Value (Builtin builtin_minus) ;
  (intern "*").value <- Value (Builtin builtin_mult) ;
  (intern "=").value <- Value (Builtin builtin_numbers_equal) ;
  (intern "<").value <- Value (Builtin builtin_less) ;
  (intern "cons").value <- Value (Builtin builtin_cons) ;
  (intern "car").value <- Value (Builtin builtin_car) ;
  (intern "cdr").value <- Value (Builtin builtin_cdr) ;
  (intern "null?").value <- Value (Builtin builtin_nullp) ;
  (intern "begin").value <- Value (Builtin builtin_begin) ;
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let expr = macroexpand (Parser.expr Lexer.token lexbuf)
    in csbceval (compile expr []) []
	 (fun dummy ->
	    print_string "done.";
	    print_newline ())
	 (fun result next ->
	    print_string "solution: ";
	    prin1 result;
	    print_newline();
	    flush stdout;
	    next ())
  done;;

main();;
exit 0;;
