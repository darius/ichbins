  (define (string? x)
    (cond ((null? x) 't)
          ((char? x) 'f)
          ((char? (car x)) (string? (cdr x)))
          ('t 'f)))

  (define (string=? s t)
    (cond ((null? s) (null? t))
          ((null? t) 'f)
          ((eq? (car s) (car t)) (string=? (cdr s) (cdr t)))
          ('t 'f)))

  (define (memq? x xs)
    (cond ((null? xs) 'f)
          ((eq? x (car xs)) 't)
          ('t (memq? x (cdr xs)))))

  (define (cons! x xs-cell)
    (set-car! xs-cell (cons x (car xs-cell))))

  (define symbols
    '((t f eof-object define quote cond
       eq? null? pair? char? cons car cdr
       set-car! read-char peek-char write-char 
       abort)))

  (define (symbol? x)
    (memq? x (car symbols)))

  (define (intern s)
    (intern-lookup s (car symbols)))

  (define (intern-lookup s syms)
    (cond ((null? syms) (cons! s symbols) s)
          ((string=? s (car syms)) (car syms))
          ('t (intern-lookup s (cdr syms)))))

  (define (print x)
    (write x)
    (newline))

  (define (newline)
    (write-char \
))

  (define (write x)
    (cond ((null? x) (write-string '"()"))
          ((char? x) (write-char \\) (write-char x))
          ((string? x)
           (cond ((symbol? x) (write-string x))
                 ('t (write-char \") (write-string x) (write-char \"))))
          ('t (write-char \()
              (write (car x))
              (write-each (cdr x))
              (write-char \)))))
          
  (define (write-each xs)
    (cond ((null? xs) 'f)
          ('t (write-char \ )
              (write (car xs))
              (write-each (cdr xs)))))
          
  (define (write-string chars)
    (cond ((null? chars) 'f)
          ('t (write-char (car chars))
              (write-string (cdr chars)))))


(define (error plaint)
  (write-string plaint)
  (newline)
  (abort))          

(define (read)
  (skip-blanks (peek-char))
  (read-dispatch (read-char)))

(define (skip-blanks c)
  (cond ((char-whitespace? c)
	 (read-char)
	 (skip-blanks (peek-char)))
	('t 'ok)))

(define (char-whitespace? c)
  (memq? c '" 
	"))

(define non-symbol-chars (cons \" '"\(')"))

(define eof-object '("eof"))

(define (read-dispatch c)
  (cond ((eq? c 'f) eof-object)
	((eq? c \\) (read-char-literal (read-char)))
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \') (cons 'quote (cons (read) '())))
	((eq? c \)) (error '"Unbalanced parentheses"))
	('t (intern (cons c (read-symbol (peek-char)))))))

(define (read-char-literal c)
  (cond ((eq? c 'f) (error '"EOF in character literal"))
	('t c)))

(define (read-string c)
  (cond ((eq? c 'f) (error '"Unterminated string literal"))
	((eq? c \") '())
	('t (cons c (read-string (read-char))))))

(define (read-symbol c)
  (cond ((char-whitespace? c) '())
	((memq? c non-symbol-chars) '())
	('t (read-char) (cons c (read-symbol (peek-char))))))

(define (read-list)
  (skip-blanks (peek-char))
  (read-list-dispatch (read-char)))

(define (read-list-dispatch c)
  (cond ((eq? c 'f) (error '"Unterminated list"))
	((eq? c \)) '())
	('t (cons (read-dispatch c) (read-list)))))


(define definitions '(()))
(define global-vars '(()))
(define global-vals '(()))

(define (repl form)
  (cond ((eq? eof-object form) 'f)
        ('t (eval-form form)
            (repl (read)))))

(define (eval-form form)
  (cond ((cond ((pair? form) (eq? (car form) 'define))
	       ('t 'f))
	 (eval-define (cdr form)))
	('t (print (eval form '() '())))))

(define (eval-define defn)
  (cond ((symbol? (car defn))
	 (define-global (car defn)
			(eval (car (cdr defn)) '() '())))
	('t (cons! defn definitions))))

(define (define-global var val)
  (cons! var global-vars)
  (cons! val global-vals))

(define (eval e vars vals)
  (cond ((pair? e) 
	 (cond ((symbol? e) (lookup e vars vals))
	       ('t (eval-pair (car e) (cdr e) vars vals))))
	('t e)))

(define (eval-pair rator rands vars vals)
  (cond ((eq? rator 'quote) (car rands))
	((eq? rator 'cond) (evcond rands vars vals))
	('t (apply rator (evlis rands vars vals)))))

(define (evlis es vars vals)
  (cond ((null? es) '())
	('t (cons (eval (car es) vars vals)
		  (evlis (cdr es) vars vals)))))

(define (evcond clauses vars vals)
  (cond ((null? clauses) '"No matching cond clause yo")
	((eval (car (car clauses)) vars vals)
	 (evseq (cdr (car clauses)) vars vals))
	('t (evcond (cdr clauses) vars vals))))

(define (evseq es vars vals)
  (cond ((null? (cdr es)) (eval (car es) vars vals))
	('t (eval (car es) vars vals)
	    (evseq (cdr es) vars vals))))

(define (lookup var vars vals)
  (cond ((null? vars) (lookup1 var (car global-vars) (car global-vals)))
	((eq? var (car vars)) (car vals))
	('t (lookup var (cdr vars) (cdr vals)))))

(define (lookup1 var vars vals)
  (cond ((null? vars) (error '"Unbound variable yo"))
	((eq? var (car vars)) (car vals))
	('t (lookup1 var (cdr vars) (cdr vals)))))

(define (apply rator args)
  (cond ((eq? rator 'eq?)          (eq? (car args) (car (cdr args))))
	((eq? rator 'null?)        (null? (car args)))
	((eq? rator 'pair?)        (pair? (car args)))
	((eq? rator 'char?)        (char? (car args)))
	((eq? rator 'cons)         (cons (car args) (car (cdr args))))
	((eq? rator 'car)          (car (car args)))
	((eq? rator 'cdr)          (cdr (car args)))
	((eq? rator 'set-car!)     (set-car! (car args) (car (cdr args))))
	((eq? rator 'read-char)    (read-char))
	((eq? rator 'peek-char)    (peek-char))
	((eq? rator 'write-char)   (write-char (car args)))
	((eq? rator 'error)        (error (car args)))
	('t (call rator args (car definitions)))))

(define (call rator args defs)
  (cond ((null? defs) (error '"Unknown rator"))
	((eq? rator (car (car (car defs))))
	 (evseq (cdr (car defs)) (cdr (car (car defs))) args))
	('t (call rator args (cdr defs)))))


(repl (read))
