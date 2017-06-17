# OCamlScheme

This is a very efficient interpreter for a small statically scoped
subset of Scheme. In the hopelessly contrived recursive Fibonacci
benchmark it beats Guile by about a factor of 2.5. It operates by
compiling Scheme code into an intermediate data structure which can be
executed more efficiently. Most importantly, no symbol lookup needs to
happen during execution. I think it's a very nice little piece of
software, so I'll present the more important parts of it here.

The two main data types are `lisp_expr`, which describes all Scheme
data (including uncompiled code, which is of course just lists), and
`lisp_code`, which describes compiled code. We'll start with
`lisp_expr`:

	type lisp_expr =
		Nil
	  | Cons of lisp_expr * lisp_expr
	  | Int of int

This is simple enough.  `Nil` is the empty list, a `Cons` is a list
cell and an `Int` is an integer. We need integers so that the fib
benchmark will run. It goes on:

      | Symbol of symbol

This is a symbol, which, as we'll see below, has a name. Now it gets
more complicated, though:

      | Builtin of (lisp_expr array -> lisp_expr)

A builtin is a function that's built into the Scheme interpreter,
hence the name. A more modern name would probably be "native
function". For example, the function `+` for adding integers is a
builtin. Builtins cannot be written in Scheme, but they can be passed
around and stored in variables (i.e., they are first class values),
which why they need to be included in this type.

In this interpreter, a builtin is simply a function taking an array of
values (the arguments) and returning a value (the result).

      | Closure of lisp_code * (lisp_expr array list)

This, finally, is a Scheme function, complete with environment. The
first part, of type `lisp_code`, is the code of the function. We'll see
the definition of that type below. The second part is the environment
of the function (or actually closure). If you're not familiar with
closures and environments, here's a short introduction:

Let's say you have this function:

	(lambda (x)
	  (lambda (y)
		(+ x y)))

It's a function that takes one argument `x` and returns another
function, or closure (a "closure" is what we call code with an
environment which it needs to execute). You can call this resulting
closure with another argument `y` and it'll give you the sum of the two
arguments. Note that you don't need to remember the argument `x` you
gave to the first function, because it's contained in the closure it
returned. The value of this argument is therefore contained in the
environment of the returned closure.

Of course environments can contain more than one value. Most
importantly, environments can be deep, as this example illustrates:

	(lambda (a b)
	  (lambda (c d)
		(lambda (e f)
		  (- (+ (- a b) (- c d)) (+ e f)))))

When you call this function you'll get a closure whose environment
contains the arguments `a` and `b`. This closure, when called, returns yet
another closure, whose environment contains the arguments `c` and `d`, but
also `a` and `b` as well.

Let's say you called the function above with the arguments `1` and `2`,
and the resulting closure with `3` and `4`. You'd get a closure whose
environment is the list of arrays `[ [| 3; 4 |]; [| 1; 2 |] ]`. The most
"recent" argument are always at the front of the list, that's why `3`
and `4` are first. Note that the environment does not contain the names
of the arguments. Instead, the compiler figures out automatically
where in the environment it needs to look for the value of an
argument. Note also that the closure does not specify how many
arguments it takes, which means that the interpreter cannot catch the
error of giving too many or too few arguments to a closure. This is
only intentional as far as I was too lazy to make it any fancier.

Next are symbols, which are easy:

	and symbol_value =
		Value of lisp_expr
	and symbol = { name: string ; mutable value: symbol_value };;

A symbol has a name, obviously, but also a value. All symbols are
stored in a global symbol table and a symbol's value is the value of
the global variable with that name. Whenever the reader encounters a
symbol, it looks it up in the symbol table and if it's not there, a
new entry is made (that process is called "interning" the
symbol). This means that whenever you use a new symbol you get a new
global variable (whose default value is `Nil`), which is usually not
what you want, but I was too lazy to change this. The easiest way
would probably be to add an `Undefined` alternative to the `symbol_value`
type, so that the interpreter can give an error message whenever an
undefined global variable is referenced.

We now come to the definition of compiled code:

	and lisp_code =
		Quote of lisp_expr

This is just a quoted expression, like `'a` (which is syntactic sugar
for `(quote a)`.

      | Global of symbol

This is a reference to a global variable.

      | Global_set of symbol * lisp_code

This is code for setting the value of a global variable, the Scheme
syntax for which is `(define name value)`.

      | Var of int * int

This is a reference to a local variable, i.e., to a value in the
current environment. The first of the two integers says how deep into
the environment the interpreter must reach, while the second integer
says which element in the resulting array is to be fetched. In the
environment example above, for example, to get to the value of `a`, we'd
have to take the second array and from that the first element, hence
the numbers would be `1` and `0` (since the first element has number `0`).

      | If of lisp_code * lisp_code * lisp_code

This is a simple conditional. The first `lisp_code` is the condition,
the second is the code for the consequent and the third for the
alternative.

      | Application of lisp_code * (lisp_code array)

This is a function application. The first `lisp_code` is supposed to
evaluate to a function, i.e., either a builtin or a closure, and the
array contains the code for the arguments.

      | Build_closure of lisp_code

This, finally, is closure-generating code, i.e., a `lambda`
expression. Again, it doesn't say how many arguments the closure is
supposed to take, which is neither an oversight nor a feature, but
merely the result of my laziness.

Now that we're through with the data structures, we're ready to
discuss the interpreter, which consists of the classic functions
`eval` and `apply`. I won't discuss the compiler because it's not as
pretty as the interpreter. The only really "complicated" thing in the
compiler is keeping track of local symbols, so that it can know where
in the environment the interpreter has to look for a variable
value. Anyway, here's the interpreter:

    let rec eval x e =

eval takes a `lisp_code` `x` and an environment `e` in which to
execute `x`. For a top-level expression, this environment is of course
the empty list. `eval` returns the result of the execution of the
code, which is a `lisp_expr`.

	  match x with
	    Quote q -> q

Quoting an expression is really easy - just return it.

      | Global { value = Value v } -> v

Referencing a global variable simply means getting its value.

      | Global_set (s, c) -> s.value <- Value (eval c e) ; Nil

Setting a global variable, on the other hand, means setting its value
(and returning `Nil`). The value it is set to is the result of executing
the corresponding code in the current environment.

      | Var (depth, index) -> (nth e depth).(index)

Since we know exactly where to look for in the environment for a local
variable, this is really easy. First we select the array out of the
list and then we select the element out of the array.

	  | If (cond, cons, alt) -> if eval cond e == Nil then
								  eval alt e
								else
								  eval cons e

This is one of the two more complicated rules. First we evaluate the
condition. If it's `Nil`, i.e., false, we evaluate the alternative,
otherwise, i.e., if it's true, we evaluate the consequent.

	  | Application (f, args) -> apply (eval f e)
									   (Array.map (function a -> eval a e) args)

Applying a function means first evaluating the function and all its
arguments, and then calling `apply`, which performs the application once
we know the function and the value of its arguments. We'll investigate
`apply` below (it's very simple).

      | Build_closure c -> Closure (c, e)

Building a closure just means putting its code and the current
environment into a `Closure`.

Here's `apply`, which takes a function (in the form of a `lisp_expr`) and
an array of arguments:

	and apply f a =
	  match f with
		Builtin b -> b a

The function can be a builtin, in which case we just use the native
OCaml function application.

      | Closure(c, e) -> eval c (a :: e)

The function can also be a closure, in which case we need to make a
new environment for it first, which really only means putting the
argument array in front of the closure's existing environment. Then we
evaluate its code in this new environment.

      | _ -> raise Hell;;

Applying anything else (like an integer) is an error.

## Compiling

Make sure you have OCaml installed and type

    make

## Usage

Example:

    ./lisp <test.scm

## License

This program is licenced under the GNU General Public License.  See
the file `COPYING` for details.

---
Mark Probst <mark.probst@gmail.com>
