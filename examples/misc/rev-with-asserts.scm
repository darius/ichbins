(define else 't)
(define nil '())
(define linefeed \
 )

(define (write-string x)
  (cond ((null? x) 't)
        ((pair? x) (write-char (car x)) (write-string (cdr x)))))

(define (write-line x)
  (write-string x)
  (write-char linefeed))

(define (error complaint)
  (write-line complaint)
  (abort))

(define (assert x complaint)
  (cond (x 't)
        (else (write-string "Assertion failure: ")
              (error complaint))))

(define (list1 x) (cons x nil))

(assert (list=? (list1 '1) '(1)) "Make singletons.")

(define (list=? x y)
  (cond ((null? x) (null? y))
        ((null? y) 'f)
        ((eq? (car x) (car y)) (list=? (cdr x) (cdr y)))
        (else 'f)))

(assert (list=? nil nil) "Empty lists are equal.")
(assert (list=? '(1) '(1)) "Singletons are equal.")
(assert (list=? '(1 2 3) '(1 2 3)) "Lists are equal.")
(assert (not (list=? nil '(1 2 3))) "List not equal nil.")
(assert (not (list=? '(1 2 3 4) '(1 2 3 4 5))) "Lists differ.")

(define (not x)
  (cond ((null? x) 't)
        (x 'f)
        (else 't)))

(assert (eq? (not 't) 'f) "Not true is false")
(assert (eq? (not 'f) 't) "Not false is true")

(assert (eq? (not "a") 'f) "Not a char is false")
(assert (eq? (not '(1 2 3)) 'f) "Not a list is false")
(assert (eq? (not '()) 't) "Not an empty list is true")
(assert (eq? (not nil) 't) "Not nil is true")

(define (do-reverse-line c line)
  (cond ((eq? c linefeed)
         (write-line line)
         (do-reverse-line (read-char) nil))
        ((peek-char)
         (do-reverse-line (read-char) (cons c line)))
        (else 'f)))

(do-reverse-line (read-char) nil)
