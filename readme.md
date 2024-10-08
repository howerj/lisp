# Embeddable Lisp Interpreter

* Author: Richard James Howe
* License: Public Domain / 0BSD
* Email: <mailto:howe.r.j.89@gmail.com>
* Repo: <https://github.com/howerj/lisp>

This is an incredibly simple [lisp interpreter](https://en.wikipedia.org/wiki/Lisp_%28programming_language%29)
written in C structured as a [header-only library](https://en.wikipedia.org/wiki/Header-only)
that is extensible by the user. It is minimalistic but usable (as an embedded
lisp).

The language consists of the following built in functions:

* `if`, `(if cond exp1 exp2)`, if "cond" is true evaluate and return "exp1"
  else "exp2".
* `pgn`, `(pgn x0 x1 ... xn)`, evaluate a list of expressions and return
  last one.
* `do`, `(do (cond) exp...)`, loop evaluating "exp" until "cond" is false,
  return last evaluation or nil in case of no evaluations.
* `fn`, `(fn (args...) body...)` or (fn varg body), create a new function
* `def`, `(def symbol expr)`, define a new symbol, adding a new symbol and
  not replacing any previous definitions. 
* `quote`, `(quote x)`, prevent evaluation of "x"
* `set`, `(set x y)`, set "x" to "y", creating "x" if it does not exist, 
  `(set x)` can be used to check if the symbol exists. `(set x y nil)` can
  be used to perform a set in the top level environment.
* `cons`, `(cons 'x 'y)`, form a new node
* `car`, `(cdr '(x y))`, get first element in a cons list
* `cdr`, `(cdr '(x y))`, get second element in a cons list
* `type`, `(type 'x)`, get the type of an expression as a number.
* `add`, `sub`, `band`, `bor`, `bxor`, `mul`, `div`, `mod`, `lls`, `lrs`, `min`,
  `max` are the arithmetic and bitwise operators
* `eq`, `neq`, `less`, `leq`, `more`, `meq`, the comparison operators
* `fexpr`, create a function which passes its arguments un-evaluted 
* `eval`, `(eval '(expr) '(optional-env))`
* `assoc`, `(assoc 'x '(assoc-list))`
* `evlis`, `(eval '(expr expr...) '(optional-evn))`
* `expand`, `(expand '(expr) '(optional-env))`
* `gensym`, `(gensym)`
* `env`, `(env)`, `(env t)`, `(env nil)`, `(env proc)`
* `!`, `(!)`, signal an error condition. This happens on many error
conditions with no way to tell what caused it. It usually causes any
expression to return an error as well.
* `%`, signal EOF when reading.
* `t`, true, although in this lisp anything not nil is true including 0
* `nil`/`()`, false, end of a list

And some extra functions defined in `main()` that are reliant on `<stdio.h>`:

* `in`, `(in)`, read in an expression from input
* `out`, `(out 'x)`, write an expression to output

Some syntax:

* +/- 0...9 -> Numbers
* Symbols are pretty much anything apart from a number and "',@()".
* () -> Nil
* 'x -> (quote x)
* Quasi-quoting is supported, e.g. `(def x 1)  @(x ,x)` returns `(x 1)`. ",@"
  is also supported to splice in lists.

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
  cell and the number value of a number. No type checking is done deliberately
  to make this as flexible as possible. You can define a set of functions that 
  does type checking if needed.
* To catch an error you can use `(eq '! EXPR)`. 
* This program has been fuzzed with [American Fuzzy Lop](https://lcamtuf.coredump.cx/afl/),
  tested with Valgrind and contains a fair few assertions so it should be fairly robust. Out of
  memory conditions are also tested for with a custom allocator.
* Internally, most lisp cells consists of a structure consisting of two or three 
  pointer sized objects, one value containing a tag value (which contains bits
  for object type, garbage collection and object length) and the other one or
  two containing data. A cons cell consists three pointer sized values, a tag and
  two pointers. Ideally it would contain only two pointers, and there are
  several ways of doing this, you can either steal bits from the pointer value
  for tag bits given the assumption that all allocated values are aligned on a
  machine word size boundary, or keep certain values in special areas of memory
  (thus anything in one array must be a cons cell, anything in another must be
  an integer, etcetera). This is not done for maximum portability, but many
  lisp implementations do things like this in order to improve memory efficiency.
* Adding [fexprs](https://en.wikipedia.org/wiki/Fexpr), with a new `lambda`
  for them, would be much easier to add than macros, although less useful. It
  is basically a function that does not evaluate its arguments.
* There are many things "missing" from this project in an effort to keep it
  small and simple, no prompt, colorization, command line options, command
  line history (use [rlwrap](https://linux.die.net/man/1/rlwrap) or integrate
  [linenoise](https://github.com/antirez/linenoise) yourself), environment
  variables to control options, and other niceties.
* [klisp](http://t3x.org/klisp) is a tiny public domain lisp with more 
  functionality (although it has other limitations) that concepts could be
  borrowed from, specifically around macros.
* Pointers are used in the implementation, instead there are advantages of using
  integers (especially integers of type `intptr_t`) and offsets into
  an array such as; being able to serialize the LISP interpreters state,
  being able to borrow bits from pointers for other purposes, ability 
  to use the location of an object as information about the object (e.g 
  all objects in this area of type X).
* If you do not like the formatting, you could indent `lisp.h` with 
  `indent -linux -nut -l 200 lisp.h` (you have to temporarily comment out
  the C++ `extern` line, otherwise the formatting is a bit off).

## Bugs and Limitations

* Maximum recursion depth is set by a compile time macro (>16). In the commit
  history is an attempt to convert the reader and writer (the writer was
  successfully converted) into a state-machine driven function that used an
  explicit stack instead of the C stack, the code was far more complex however,
  even though doing this (especially if `lisp_eval` could be converted) would
  have many benefits. If `lisp_eval` were to be converted to use a
  state-machine and an explicit stack we could yield and restore from the
  lisp interpreter, and support much larger recursion depths. It would also
  help in closure formation.
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
* The system lacks image files, implementing them would be serializing
  pointers (which can be avoided by using indexes instead), instead
  serialization could be achieved by reading and writing s-expressions to disk
  instead.
* More information about pointer tagging in general can be found at
  <https://coredumped.dev/2024/09/09/what-is-the-best-pointer-tagging-method/>,
  which has been discussed at: <https://news.ycombinator.com/item?id=41488377>.

# References

* <https://justine.lol/sectorlisp2/>
* <https://www.lispmachine.net/books/LISP_1.5_Programmers_Manual.pdf>
* <https://en.wikipedia.org/wiki/Lisp_(programming_language)>
* <http://t3x.org/klisp> (there are multiple great Lisp implementations on this
  site, in the public domain as well, one of which is VM based).

