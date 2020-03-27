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

  (define symbols '((f)))

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
          
  (print '())
  (print '"abc")
  (print '(\a (\b)))
  (print (cons (intern '"hello") '(f "world")))
