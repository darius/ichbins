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

  (to (memq? x xs)
    (cond ((null? xs) 'f)
          ((eq? x (car xs)) 't)
          ('t (memq? x (cdr xs)))))

  (to (cons! x xs-cell)
    (set-car! xs-cell (cons x (car xs-cell))))

  (define symbols '((f quote)))

  (to (symbol? x)
    (memq? x (car symbols)))

  (to (intern s)
    (intern-lookup s (car symbols)))

  (to (intern-lookup s syms)
    (cond ((null? syms) (cons! s symbols) s)
          ((string=? s (car syms)) (car syms))
          ('t (intern-lookup s (cdr syms)))))

  (to (print x)
    (write x)
    (newline))

  (to (newline)
    (write-char \
))

  (to (write x)
    (cond ((null? x) (write-string '"()"))
          ((char? x) (write-char \\) (write-char x))
          ((string? x)
           (cond ((symbol? x) (write-string x))
                 ('t (write-char \") (write-string x) (write-char \"))))
          ('t (write-char \()
              (write (car x))
              (write-each (cdr x))
              (write-char \)))))
          
  (to (write-each xs)
    (cond ((null? xs) 'f)
          ('t (write-char \ )
              (write (car xs))
              (write-each (cdr xs)))))
          
  (to (write-string chars)
    (cond ((null? chars) 'f)
          ('t (write-char (car chars))
              (write-string (cdr chars)))))


(to (error plaint)
  (write-string plaint)
  (newline)
  (abort))          

(to (read)
  (skip-blanks (peek-char))
  (read-dispatch (read-char)))

(to (skip-blanks c)
  (cond ((char-whitespace? c)
	 (read-char)
	 (skip-blanks (peek-char)))
	('t 'ok)))

(to (char-whitespace? c)
  (memq? c '" 
	"))

(define non-symbol-chars (cons \" '"\(')"))

(define eof-object '("eof"))

(to (read-dispatch c)
  (cond ((eq? c 'f) eof-object)
	((eq? c \\) (read-char-literal (read-char)))
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \') (cons 'quote (cons (read) '())))
	((eq? c \)) (error '"Unbalanced parentheses"))
	('t (intern (cons c (read-symbol (peek-char)))))))

(to (read-char-literal c)
  (cond ((eq? c 'f) (error '"EOF in character literal"))
	('t c)))

(to (read-string c)
  (cond ((eq? c 'f) (error '"Unterminated string literal"))
	((eq? c \") '())
	('t (cons c (read-string (read-char))))))

(to (read-symbol c)
  (cond ((char-whitespace? c) '())
	((memq? c non-symbol-chars) '())
	('t (read-char) (cons c (read-symbol (peek-char))))))

(to (read-list)
  (skip-blanks (peek-char))
  (read-list-dispatch (read-char)))

(to (read-list-dispatch c)
  (cond ((eq? c 'f) (error '"Unterminated list"))
	((eq? c \)) '())
	('t (cons (read-dispatch c) (read-list)))))


(print (read))
(print (read))
