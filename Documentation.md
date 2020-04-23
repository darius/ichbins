XXX give the writeup a proper name

# The language

Ichbins implements (and is written in) a dialect of Lisp, looking most
like Scheme on the surface, but drastically cut down. In this writeup
I'll assume you're already familiar with Lisp or Scheme.


## Datatypes

In Ichbins there are just two basic datatypes: characters and lists. A
character looks like `\x`, while a list is written out the same way as
in other Lisps. For instance, this is a list of two lists of
characters: `((\h \e \l \l \o) (\w \o \r \l \d))`.

For convenience, a list of characters can also be written as a
double-quoted string: the above example is the same as `("hello"
"world")`.

Ichbins also has a notion of symbols, but unlike classical Lisp
symbols they're not distinct from strings. `(hello world)` is *also* a
list of two strings (and thus a list of two lists of
characters). What's different is that those two strings are
'interned': they're registered in the system's table of symbols. The
predicate `(symbol? x)` checks if *x* is in that table. So, `(car
'hello)` evaluates to `\h`, the first character of `"hello", because
the symbol `hello` is also the string `"hello"`.

Why such a silly design? Because the fewer the datatypes, the less
code to implement them. In this project for any tradeoff yielding less
code, unless it would give up readability, I took it. What was given
up this time, in choosing to minimize the datatypes, was more like
writability: it became easier to screw up. (Of course this means
*adversarial* readability suffers, but we're going to assume you trust
the author.)

Finally, a boolean value is represented as a symbol: `f` for false,
`t` for true.

Symbols are case-sensitive.


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

Of course there's `'foo` as read syntax for `(quote foo)`.


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
(write-char \ )
(say verb)
```

So variable definitions look like Scheme's; so do procedure
definitions but with `to` in place of `define`.

To run a program:

- First collect all the procedure definitions, associating each procedure name with its parameters and body.
- Collect all the global variable definitions, and evaluate them in
order. (If one of them refers to one not yet defined, that's
undefined behavior, which in practice may blow up the process.)
- Evaluate each of the top-level expressions in order.


## Primitive procedures

These are built in:

- `eq?`
- `null?`, `char?`, `pair?`
- `cons`, `car`, `cdr`, `set-car!`
- `write-char`, `read-char`, `peek-char`
- `abort` (like C's `exit(1)`)

Scheme's `peek-char` returns the next character from `stdin` but
without consuming it. It's like C's `getchar()` plus `ungetc()`.


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
