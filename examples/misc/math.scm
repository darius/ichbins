(define numbers '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define else 't)
(define nil '())
(define linefeed \
 )

(define (write-string x)
  (cond ((null? x) 't)
        ((pair? x) (write-char (1st x)) (write-string (rest x)))
        (else (error "Invalid argument to write-string."))))

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

(define (singleton? x)
  (cond ((pair? x) (null? (rest x)))
        (else 'f)))

(assert (not (singleton? nil)) "Nil is not a singleton")
(assert (singleton? '(x)) "One item is a singleton")
(assert (not (singleton? '(x y))) "A two-element list is not a singleton")

(define (list1 x) (cons x nil))

(assert (list=? (list1 '1) '(1)) "Make singletons.")

(define (list=? x y)
  (cond ((null? x) (null? y))
        ((null? y) 'f)
        ((eq? (1st x) (1st y)) (list=? (rest x) (rest y)))
        (else 'f)))

(assert (list=? nil nil) "Empty lists are equal.")
(assert (list=? '(1) '(1)) "Singletons are equal.")
(assert (list=? '(1 2 3) '(1 2 3)) "Lists are equal.")
(assert (not (list=? nil '(1 2 3))) "List not equal nil.")
(assert (not (list=? '(1 2 3 4) '(1 2 3 4 5))) "Lists differ.")

(define (memq? x xs)
  (cond ((null? xs) 'f)
        ((eq? x (1st xs)) 't)
        (else (memq? x (rest xs)))))

(assert (memq? '1 '(1 2 3)) "Can find first element")
(assert (memq? '2 '(1 2 3)) "Can find middle element")
(assert (memq? '3 '(1 2 3)) "Can find last element")
(assert (not (memq? '4 '(1 2 3))) "Can't find missing element")

(define (append xs ys)
  (cond ((null? xs) ys)
        (else (cons (car xs) (append (cdr xs) ys)))))

(assert (list=? (append '(1 2) '(3 4)) '(1 2 3 4)) "Appendage.")
(assert (list=? (append "abc" "def") "abcdef") "String joiner.")

(define (reverse xs)
  (revappend xs nil))

(assert (list=? (reverse '(1 2 3)) '(3 2 1)) "We can reverse things.")
(assert (list=? (reverse '(1)) '(1)) "The reverse of a singleton is itself")

(define (revappend xs ys)
  (cond ((null? xs) ys)
        (else (revappend (rest xs) (cons (1st xs) ys)))))

(assert (list=? (revappend '(1 2 3) '(1 2 3)) '(3 2 1 1 2 3)) "Revappend.")

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

(define (bool x) (not (not x)))

(define (or x)
  (cond ((singleton? x) (1st x))
        ((not (bool (1st x))) (or (rest x)))
        (else (1st x))))

(assert (eq? (or '(1)) '1) "Or returns singleton item")
(assert (eq? (or '(f f f 2 f 4)) '2) "Or returns first true")
(assert (eq? (or '(f f f f f ())) nil) "Or returns last false")

(define (and x)
  (cond ((singleton? x) (1st x))
        ((bool (1st x)) (and (rest x)))
        (else (1st x))))

(assert (eq? (and '(1)) '1) "And returns singleton item")
(assert (eq? (and '(1 3 4 6 67 2)) '2) "And returns last true")
(assert (eq? (and '(1 2 () 4 f)) nil) "And returns first false")
(assert (eq? (and '(1 2 f 4 ())) 'f) "And returns first false (2)")

(define (if condition then other)
  (cond ((bool condition) then)
        (else other)))

(assert (eq? (if 't '10 '20) '10) "If true, 10")
(assert (eq? (if 'f '10 '20) '20) "If false, 20")
(assert (eq? (if nil '10 '20) '20) "If nil, 20")

(define (unless condition other)
  (cond ((bool condition) 'f)
        (else other)))

(assert (eq? (unless 't '10) 'f) "Unless true")
(assert (eq? (unless 'f '20) '20) "Unless false")
(assert (eq? (unless nil '20) '20) "Unless nil")

(define (1st x) (car x))
(define (2nd x) (car (cdr x)))
(define (rest x) (cdr x))

(define (do-inc x increments)
  (cond ((eq? x (1st increments)) (2nd increments))
        (else (do-inc x (rest increments)))))

(define (inc x)
  (do-inc x numbers))

(assert (eq? (inc '0) '1) "Zero inc is one.")
(assert (eq? (inc '1) '2) "One inc is two.")
(assert (eq? (inc '2) '3) "Two inc is three.")
(assert (eq? (inc '3) '4) "Three inc is four.")
(assert (eq? (inc '4) '5) "Four inc is five.")

(define (do-dec x increments)
  (cond ((eq? x (2nd increments)) (1st increments))
        (else (do-dec x (rest increments)))))

(define (dec x)
  (do-dec x numbers))

(assert (eq? (dec '1) '0) "One dec is zero.")
(assert (eq? (dec '2) '1) "Two dec is one.")
(assert (eq? (dec '3) '2) "Three dec is two.")
(assert (eq? (dec '4) '3) "Four dec is three.")
(assert (eq? (dec '5) '4) "Five dec is four.")

(define (plus x y)
  (cond ((eq? y '1) (inc x))
        (else (plus (inc x) (dec y)))))

(assert (eq? (plus '1 '1) '2) "One + one is two.")
(assert (eq? (plus '5 '5) '10) "Five + five is ten.")

(define (minus x y)
  (cond ((eq? y '1) (dec x))
        (else (minus (dec x) (dec y)))))

(assert (eq? (minus '1 '1) '0) "One - one is zero.")
(assert (eq? (minus '5 '5) '0) "Five - five is zero.")
(assert (eq? (minus '5 '4) '1) "Five - four is one.")
(assert (eq? (minus '15 '4) '11) "Fifteen - four is eleven.")

(define (sum x)
  (cond ((singleton? x) (1st x))
        ((pair? x) (plus (1st x) (sum (rest x))))))

(assert (eq? (sum '(1)) '1) "One is one.")
(assert (eq? (sum '(1 1)) '2) "One plus one is two.")
(assert (eq? (sum '(1 2 3 4)) '10) "Sum to ten.")

(define (do-min x y increments)
  (cond ((null? increments) 'f)
        ((eq? x (1st increments)) x)
        ((eq? y (1st increments)) y)
        (else (do-min x y (rest increments)))))

(define (min x y) (do-min x y numbers))

(assert (eq? (min '5 '9) '5) "Five less than nine.")
(assert (eq? (min '9 '2) '2) "Two less than nine.")

(define (max x y) (do-min x y (reverse numbers)))

(assert (eq? (max '5 '9) '9) "Nine more than five.")
(assert (eq? (max '11 '2) '11) "Eleven more than two.")

(write-line "Success!")
