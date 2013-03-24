(define else 't)
(define nil '())
(define linefeed \
 )

(define (write-string x)
  (cond ((null? x) 't)
        ((pair? x) (write-char (car x)) (write-string (cdr x)))))

(define (do-reverse-line c line)
  (cond ((eq? c linefeed)
         (write-string line)
         (write-char linefeed)
         (do-reverse-line (read-char) nil))
        ((peek-char)
         (do-reverse-line (read-char) (cons c line)))
        (else 'f)))

(do-reverse-line (read-char) nil)
