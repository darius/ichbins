(define else 't)
(define nil '())
(define linefeed \
 )

(define (write-string x)
  (cond ((null? x) 't)
        ((pair? x) (write-char (car x)) (write-string (cdr x)))
        (else 'f)))

(define (write-line x)
  (write-string x)
  (write-char linefeed))

(define (list=? x y)
  (cond ((null? x) (null? y))
        ((null? y) 'f)
        ((eq? (car x) (car y)) (list=? (cdr x) (cdr y)))
        (else 'f)))

(define (append xs ys)
  (cond ((null? xs) ys)
        (else (cons (car xs) (append (cdr xs) ys)))))

(define (not x)
  (cond ((null? x) 't)
        (x 'f)
        (else 't)))

(define (do-process-lines c line prev)
  (cond ((eq? c linefeed)
         (cond ((not (list=? line prev)) (write-line line)))
         (do-process-lines (read-char) nil line))
        ((peek-char)
         (do-process-lines (read-char) (append line (cons c nil)) prev))
        (else 'f)))

(do-process-lines (read-char) nil nil)
