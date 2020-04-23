# The language

Ichbins implements (and is written in) a dialect of Lisp, looking most
like Scheme on the surface, but drastically cut down. In this writeup
I'll assume you're already familiar with Lisp or Scheme.


## Datatypes

In Ichbins there are just two basic datatypes: characters and lists. A
character looks like `\x`, while a list is written out the same way as
in other Lisps. For instance, `((\h \e \l \l \o) (\w \o \r \l \d))` is
a list of two lists of characters.

A list of characters can also be written more compactly as a
double-quoted string: `("hello" "world")` is the same list of lists,
just in different notation.

Ichbins also has a notion of symbols, but unlike classical Lisp
symbols they're not distinct from strings. `(hello world)` is *also* a
list of two strings (and thus a list of two lists of
characters). What's different is that those two strings are
'interned': they're registered in the system's table of symbols. The
predicate `(symbol? x)` checks if *x* is in that table. So, `(car
'hello)` evaluates to `\h`, the first character of `"hello", because
the symbol `hello` is also the string `"hello"`.

Why such a silly design? Because the fewer the datatypes, the less
code to implement them. In this project I'd take any tradeoff yielding
less code, unless it would give up readability. What was given up this
time, in choosing to minimize the datatypes, was more like
writability: it became easier to screw up. (Of course this fragility
hurts *adversarial* readability, but we're going to assume you trust
the author.)

Finally, for the same reason of parsimony a boolean value is
represented as a symbol: `f` for false, `t` for true.

Symbols are case-sensitive.


## Read syntax

Ichbins's reader doesn't support dotted pairs. It does expand `'foo`
into `(quote foo)`.

Strings can include escape sequences, but only `\\` and `\"` are
useful.

Even comments are not supported. (If desperate, you can fake them with
literal strings.)


## Expressions

Expressions are like any Lisp's:

- `(quote foo)` --> `foo`
- `variable` --> the value bound to `variable`
- `(f x y)`: call function `f` with the values of `x` and `y`
- `(cond (p1 e1) (p2 e2) ...)`: as usual, eval `p1`, and unless it's
false return the value of `e1`; or if it is false then continue with
`p2` and `e2`, etc. (You can actually have any number of `e`'s in each
clause; the value of the last one is the result.)

The `(f x y)` expressions are distinguished by `f` being a symbol
other than `quote` or `cond`. Ichbins is a first-order language: there
are no lambda expressions.

An expression of any other form -- for example, the string `"hey"` --
is treated as a constant.


## Programs

A program is a sequence of top-level forms. A top-level form is a
variable definition, a procedure definition, or an expression. Here's
one program:

```
(define noun "Ich")
(define verb "bins")
(to (say chars)
  (cond ((pair? chars)
         (write-char (car chars))
         (say (cdr chars)))))
(say noun)
(say " ")
(say verb)
```

So variable definitions look like Scheme's; so do procedure
definitions but with `to` in place of `define`. A procedure's body may
be a sequence of expressions, not just one.

To run a program:

- First collect all the procedure definitions, associating each
procedure name with its parameters and body.
- Collect all the global variable definitions, and evaluate them in
order. (If one of them refers to one not yet defined, that's
undefined behavior, which in practice may blow up the process.)
- Evaluate each of the top-level expressions in order.


## Primitives

These procedures are built in:

- `eq?`
- `null?`, `char?`, `pair?`
- `cons`, `car`, `cdr`, `set-car!`
- `write-char`, `read-char`, `peek-char`
- `abort` (like C's `exit(1)`)

There are no I/O port objects: `write-char` writes to stdout,
`read-char` reads from stdin.

The value of `(read-char)` is either a character or `f` (false) for
end-of-file.

Like Scheme's, `peek-char` acts like `read-char` but without consuming
the next character. (Or like `getchar()` plus `ungetc()` in C.)

XXX eof-object is different in boot-terp


# The bootstrap interpreter

To start the cycle of life there was an interpreter in C, in
`boot-terp/`. It's unmaintained now; I might someday go back to make
sure it can still build the compiler (and to fix the segfaults I'm
guilty of; I suppose that must come down to undefined behavior which
used to work in the gcc of the time).

In `boot-terp/t` is a sequence of unit tests for the interpreter,
which might be of use to clarify the above language spec. It can be
run via `make test`.


# The compiler

TODO flesh out from Overview.text
