(define linefeed \
 )

(to (error complaint)
  (write-string complaint)
  (write-char linefeed)
  (abort))

(to (list1 z)     (cons z '()))
(to (list3 x y z) (cons x (cons y (cons z '()))))

(to (append xs ys)
  (cond ((null? xs) ys)
        ('t (cons (car xs) (append (cdr xs) ys)))))

(to (reverse xs)
  (append-reverse xs '()))

(to (append-reverse xs ys)
  (cond ((null? xs) ys)
	('t (append-reverse (cdr xs) (cons (car xs) ys)))))

(to (memq? x xs)
  (cond ((null? xs) 'f)
	((eq? x (car xs)) 't)
	('t (memq? x (cdr xs)))))

(to (length xs)
  (list1 (length-digit xs "0123456789")))

(to (length-digit xs digits)
  (cond ((null? xs) (car digits))
	('t (length-digit (cdr xs) (cdr digits)))))

(to (string? x)
  (cond ((null? x) 't)
	((char? x) 'f)
	((char? (car x)) (string? (cdr x)))
	('t 'f)))

(to (string=? s t)
  (cond ((null? s) (null? t))
        ((null? t) 'f)
	((eq? (car s) (car t)) (string=? (cdr s) (cdr t)))
	('t 'f)))

(to (write-string chars)
  (cond ((pair? chars)
	 (write-char (car chars))
	 (write-string (cdr chars)))))

(to (cons! x xs-box)
  (set-car! xs-box (cons x (car xs-box))))

(to (adjoin! x xs-box)
  (cond ((eq? 'f (memq? x (car xs-box)))
	 (cons! x xs-box))))


(define primitives '(eq? null? pair? char? cons car cdr set-car!
                     read-char peek-char write-char abort))

(define symbols-box (list1 (append '(t f define to quote cond) primitives)))
(to (symbol? x) (memq? x (car symbols-box)))
(to (intern s)  (interning s (car symbols-box)))

(to (interning s symbols)
  (cond ((null? symbols) (cons! s symbols-box) s)
	((string=? s (car symbols)) (car symbols))
	('t (interning s (cdr symbols)))))


(to (read)
  (read-dispatch (skip-blanks (read-char))))

(to (skip-blanks c)
  (cond ((memq? c whitespace-chars) (skip-blanks (read-char)))
	('t c)))

(define whitespace-chars (cons linefeed " 	"))
(define non-symbol-chars "\"\\(')")

(define eof-object '("eof"))

(to (read-dispatch c)
  (cond ((eq? c 'f) eof-object)
	((eq? c \\) (read-char-literal (read-char)))
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \') (cons 'quote (list1 (read))))
	((eq? c \)) (error "Unbalanced parentheses"))
	('t (intern (cons c (read-symbol (peek-char)))))))

(to (read-char-literal c)
  (cond ((eq? c 'f) (error "EOF in character literal"))
	('t c)))

(to (read-string c)
  (cond ((eq? c 'f) (error "Unterminated string literal"))
	((eq? c \") '())
        ((eq? c \\) (cons (read-char) (read-string (read-char))))
	('t (cons c (read-string (read-char))))))

(to (read-symbol c)
  (cond ((memq? c whitespace-chars) '())
	((memq? c non-symbol-chars) '())
	('t (read-char) (cons c (read-symbol (peek-char))))))

(to (read-list)
  (read-list-dispatch (skip-blanks (read-char))))

(to (read-list-dispatch c)
  (cond ((eq? c 'f) (error "Unterminated list"))
	((eq? c \)) '())
	('t (cons (read-dispatch c) (read-list)))))


(to (emit1         z k) (append z (cons linefeed k)))
(to (emit3     x y z k) (append x (append y (emit1 z k))))
(to (emit5 v w x y z k) (append v (append w (emit3 x y z k))))


(to (compile)
  (write-string (read-compile-loop '((t f)) (read) '() '() postlude-lines)))

(to (read-compile-loop syms form var-defs exprs k)
  (cond ((eq? eof-object form)
         (compile-startup syms (reverse var-defs)
           (compile-proc syms '(main) (reverse exprs) k)))
        ((pair? form)
         (cond ((eq? 'define (car form))
                (read-compile-loop syms (read) (cons form var-defs) exprs k))
               ((eq? 'to (car form))
                (read-compile-loop syms (read) var-defs exprs
                  (compile-proc syms (car (cdr form)) (cdr (cdr form)) k)))
               ('t (read-compile-loop syms (read) var-defs (cons form exprs) k))))
        ('t (read-compile-loop syms (read) var-defs (cons form exprs) k))))

(to (compile-startup syms var-defs k)
  (compile-symbols syms var-defs
    (compile-defs syms var-defs
      (emit1 "  bp = sp + 1; goto proc_main;" k))))

(to (compile-symbols syms var-defs k)
  (emit-enum "var_" (append (map-def.name (make-symbol-defs (car syms)))
                            (map-def.name var-defs))
    (emit1 prelude-lines
      (compile-defs syms (make-symbol-defs (car syms)) k))))

(to (compile-defs syms defs k)
  (cond ((null? defs) k)
        ('t (compile-def syms (def.name (car defs)) (def.expr (car defs))
              (compile-defs syms (cdr defs) k)))))

(to (express syms x)
  (cond ((symbol? x) (adjoin! x syms) (symbol->var x))
	((pair? x) (express-pair syms x))
	('t x)))

(to (express-pair syms x)
  (list3 'cons (express syms (car x)) (express syms (cdr x))))

(to (make-symbol-defs symbols)
  (cond ((null? symbols) '())
	('t (cons (list3 'define (symbol->var (car symbols))
			 (express-pair '() (car symbols)))
		  (make-symbol-defs (cdr symbols))))))

(to (symbol->var sym)
  (intern (cons \. sym)))

(to (def.name def)          (car (cdr def)))
(to (def.expr def)     (car (cdr (cdr def))))

(to (map-def.name defs)
  (cond ((null? defs) '())
	('t (cons (def.name (car defs))
		  (map-def.name (cdr defs))))))

(to (compile-def syms name e k)
  (compile-expr syms e '() 'f
    (emit3 "  assert(var_" (c-id name) " == sp);" k)))

(to (compile-proc syms header body k)
  (emit1 ""
    (emit3 "proc_" (c-id (car header)) ": {"
      (emit-enum "arg_" (cdr header)
        (emit3 "  assert(" (length (cdr header)) " == sp - bp + 1);"
          (compile-seq syms body (cdr header) 't
            (emit1 "}" k)))))))

(to (compile-seq syms es vars tail? k)
  (cond ((null? (cdr es)) (compile-expr syms (car es) vars tail? k))
	('t (compile-expr syms (car es) vars 'f
              (emit1 "  --sp;"
                (compile-seq syms (cdr es) vars tail? k))))))

(to (compile-exprs syms es vars k)
  (cond ((null? es) k)
        ('t (compile-expr syms (car es) vars 'f
              (compile-exprs syms (cdr es) vars k)))))

(to (compile-expr syms e vars tail? k)
  (cond ((char? e)
	 (compile-value "entag(a_char, '" (c-char-literal e) "')" vars tail? 
           k))
	((null? e) (compile-value "" "nil" "" vars tail? k))
	((symbol? e)
	 (cond ((memq? e vars)
		(compile-value "stack[bp + arg_" (c-id e) "]" vars tail? k))
	       ('t (compile-value "stack[var_" (c-id e) "]" vars tail? k))))
        ((string? e) (compile-expr syms (express syms e) vars tail? k))
	('t (compile-pair syms (car e) (cdr e) vars tail? k))))

(to (compile-value lit1 lit2 lit3 vars tail? k)
  (emit5 "  push(" lit1 lit2 lit3 ");"
    (maybe-return vars tail? k)))

(to (maybe-return vars tail? k)
  (cond (tail? (emit3 "  sp -= " (length vars) ";"
                 (emit3 "  stack[sp] = stack[sp + " (length vars) "];"
                   (emit1 "  return;" k))))
        ('t k)))

(to (compile-pair syms rator rands vars tail? k)
  (cond ((eq? rator 'cond) (compile-cond syms rands vars tail? k))
        ((eq? rator 'quote)
         (compile-expr syms (express syms (car rands)) vars tail? k))
	('t (compile-exprs syms rands vars
              (compile-call rator (length rands) vars tail? k)))))

(to (compile-cond syms clauses vars tail? k)
  (cond ((null? clauses) (compile-value "" "sym_f" "" vars tail? k))
	('t (compile-expr syms (car (car clauses)) vars 'f
              (emit1 "  if (sym_f != pop()) {"
                (compile-seq syms (cdr (car clauses)) vars tail?
                  (emit1 "  } else {"
                    (compile-cond syms (cdr clauses) vars tail?
                      (emit1 "  }" k)))))))))

(to (compile-call rator n-args vars tail? k)
  (cond ((memq? rator primitives)
	 (emit5 "  prim" n-args "_" (c-id rator) "();"
           (maybe-return vars tail? k)))
	(tail?
         (emit5 "  TAILCALL(proc_" (c-id rator) ", " n-args ");" k))
        ('t
         (emit5 "  run(&&proc_" (c-id rator) ", sp - " n-args " + 1);" k))))

(define c-char-map-domain  (list3 linefeed \'     \\))
(define c-char-map-range   (list3 "\\n"   "\\'" "\\\\"))
(to (c-char-literal c)
  (translit c (list1 c) c-char-map-domain c-char-map-range))

(to (c-id str)
  (cond ((null? str) '())
	('t (cons (translit (car str) (car str) c-id-map-domain c-id-map-range)
		  (c-id (cdr str))))))

(define c-id-map-domain "-!:=.><%?*_")
(define c-id-map-range  "_BCEDGLMPSU")

(to (translit x default domain range)
  (cond ((null? domain) default)
        ((eq? x (car domain)) (car range))
        ('t (translit x default (cdr domain) (cdr range)))))

(to (emit-enum prefix names k)
  (cond ((null? names) k)
        ('t (append "  enum {" (comma prefix names (emit1 " };" k))))))

(to (comma prefix names k)
  (cond ((null? names) k)
        ('t (cons \  (append prefix (append (c-id (car names))
              (maybe-comma prefix (cdr names) k)))))))

(to (maybe-comma prefix names k)
  (cond ((null? names) k)
        ('t (cons \, (comma prefix names k)))))

(define prelude-lines "
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned Obj;
typedef enum  { a_pair, nil, a_char } Tag;
static Tag      get_tag(Obj x)    { return 3 & x; }
static Obj      entag(Tag tag, unsigned value)
                                  { return tag | (value << 2); }
static unsigned untag(Tag tag, Obj x)
                                  { assert(tag == get_tag(x));
                                    return x >> 2; }

enum          { stack_size = 256*1024 };
static Obj      stack[stack_size];
static unsigned sp = -1;

#define         TOP               ( stack[sp] )
static Obj      pop(void)         { return stack[sp--]; }
static void     push(Obj x)       { assert(sp + 1 < stack_size);
                                    stack[++sp] = x; }

enum          { heap_size = 512*1024 };
static Obj      heap[heap_size][2];
static char     marks[heap_size];
static unsigned hp = 0;

static unsigned heap_index(Obj x) { unsigned p = untag(a_pair, x);
                                    assert(p < heap_size);
                                    return p; }
static Obj  car    (Obj x)        { return heap[heap_index(x)][0]; }
static Obj  cdr    (Obj x)        { return heap[heap_index(x)][1]; }
static void set_car(Obj x, Obj y) { heap[heap_index(x)][0] = y; }
static void mark(Obj x)           { while (get_tag(x) == a_pair
                                           && !marks[heap_index(x)]) {
                                      marks[heap_index(x)] = 1;
                                      mark(car(x));
                                      x = cdr(x); } }
static int  sweep(void)           { while (hp < heap_size && marks[hp])
                                      marks[hp++] = 0;
                                    return hp < heap_size; }
static void gc(Obj car, Obj cdr)  { unsigned i;
                                    mark(car); mark(cdr);
                                    for (i = 0; i <= sp; ++i)
                                      mark(stack[i]);
                                    hp = 0; }
static Obj cons(Obj car, Obj cdr) { if (!sweep()) {
                                      gc(car, cdr);
                                      if (!sweep()) {
                                        fprintf(stderr, \"Heap full\\n\");
                                        exit(1); } }
                                    heap[hp][0] = car;
                                    heap[hp][1] = cdr;
                                    return entag(a_pair, hp++); }

#define     sym_f                 ( stack[var_Df] )
#define     sym_t                 ( stack[var_Dt] )
static Obj  make_flag(int flag)   { return flag ? sym_t : sym_f; }

static int  read_char(void)       { int c = getchar();
                                    push(EOF == c ? sym_f : entag(a_char, c));
                                    return c; }

#define DEF(prim) static void prim(void)
DEF(prim2_eqP)        { Obj z = pop(); TOP = make_flag(TOP == z); }
DEF(prim1_nullP)      { TOP = make_flag(nil == TOP); }
DEF(prim1_charP)      { TOP = make_flag(a_char == get_tag(TOP)); }
DEF(prim1_pairP)      { TOP = make_flag(a_pair == get_tag(TOP)); }
DEF(prim2_cons)       { Obj z = pop(); TOP = cons(TOP, z); }
DEF(prim1_car)        { TOP = car(TOP); }
DEF(prim1_cdr)        { TOP = cdr(TOP); }
DEF(prim2_set_carB)   { Obj z = pop(); set_car(TOP, z); TOP = sym_f; }
DEF(prim0_read_char)  { (void) read_char(); }
DEF(prim0_peek_char)  { ungetc(read_char(), stdin); }
DEF(prim1_write_char) { putchar(untag(a_char, TOP)); TOP = sym_f; }
DEF(prim0_abort)      { exit(1); }

#define TAILCALL(label, nargs) do {                                           \\
    memmove(stack + bp, stack + sp - (nargs) + 1, (nargs) * sizeof stack[0]); \\
    sp = bp + (nargs) - 1;                                                    \\
    goto label;                                                               \\
  } while (0)

void run(void **function, int bp) {
  if (function) goto *function;")

(define postlude-lines
"}

int main() { run(NULL, 0); return 0; }
")

(compile)
