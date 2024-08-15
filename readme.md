# Embeddable Lisp Interpreter

* Author: Richard James Howe
* License: Public Domain / 0BSD
* Email: <mailto:howe.r.j.89@gmail.com>
* Repo: <https://github.com/howerj/lisp>

This is an incredibly simple [lisp interpreter](https://en.wikipedia.org/wiki/Lisp_%28programming_language%29)
written in C structured as a [header-only library](https://en.wikipedia.org/wiki/Header-only)
that is extensible by the user. It is minimalistic but usable (as an embedded
lisp).

user. The language consists of the following:

* `if`, `(if cond exp1 exp2)`, if "cond" is true evaluate and return "exp1"
  else "exp2".
* `pgn`, `(pgn x0 x1 ... xn)`, evaluate a list of expressions and return
  last one.
* `do`, `(do (cond) exp...)`, loop evaluating "exp" until "cond" is false,
  return last evaluation or nil in case of no evaluations.
* `fn`, `(fn (args) body...)`, create a new function
* `def`, `(def symbol expr)`, define a new symbol
* `quote`, `(quote x)`, prevent evaluation of "x"
* `set`, `(set x y)`, set "x" to "y"
* `cons`, `(cons 'x 'y)`, form a new node
* `car`, `(cdr '(x y))`, get first element in a cons list
* `cdr`, `(cdr '(x y))`, get second element in a cons list
* `type`, `(type 'x)`, get the type of an expression as a number.
* `add`, `sub`, `and`, `or`, `xor`, `mul`, `div`, `lls`, `lrs`, the
  arithmetic operators
* `eq`, `neq`, `less`, `leq`, `more`, `meq`, the comparison operators
* `!`, `(!)`, signal an error condition. This happens on many error
conditions with no way to tell what caused it. It usually causes any
expression to return an error as well.
* `t`, true, although in this lisp anything not nil is true including 0
* `nil`/`()`, false, end of a list

And some extra functions defined in `main()` that are reliant on `<stdio.h>`:

* `in`, `(in)`, read in an expression from input
* `out`, `(out 'x)`, write an expression to output

The C API consists of many `static inline` helper functions, some structures
which ideally would be opaque but are not because of the nature of the library,
and functions marked with `LISP_EXTERN` which form the core of the APIs
functionality.

In order to use the library you must populate the `lisp_t` with an allocator,
and to read and write s-expressions you must pass in input and output functions
to those functions. For an example of its usage see the implementation in the
header file, it includes a custom allocator that allocates objects of the size
most commonly used by the lisp interpreter and uses `realloc`/`free` as a
fallback. It also includes callbacks for input and output that are passed to
the read/write functions.

There are a handful of types defined, and it space for defining more. The
most useful addition would be strings (symbols are not strings, and symbols are
interned) but that would add a lot of size to the interpreter.

## Notes

* Arithmetic operates on the pointer value of a symbol, a function, or a cons
  cell and the number value of a number. No type checking is done. You can
  define a set of functions that does type checking if needed.
* To catch an error you can use `(eq '! EXPR)`. 

## Bugs and Limitations

* Maximum recursion depth is set by a compile time macro (>16)
* The maximum length of a string or symbol `pow((sizeof(uintptr_t) * 8) - 4, 2) - 1`
  bytes, if you include the NUL terminal symbol to maintain compatibility with
  C strings.
* I am sure that the garbage collection situation could be improved. It uses
  a primitive mark and sweep collector, along with a custom allocator that
  aids (and does not replace) the C allocation functions.
* The interpreter is noticeably slower when assertions are turn on.
* Floating point numbers and strings are missing as types. Adding these would
  make the core interpreter much more useful, with most of the functionality
  for manipulating these types ending up as extensions.

# References

* <https://justine.lol/sectorlisp2/>
* <https://www.lispmachine.net/books/LISP_1.5_Programmers_Manual.pdf>
* <https://en.wikipedia.org/wiki/Lisp_(programming_language)>

