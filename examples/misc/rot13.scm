(define alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define nycunorg "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM")

(define (do-cipher-char x letters rot)
  (cond ((null? letters) x)
        ((null? rot) x)
        ((eq? x (car letters)) (car rot))
        ('t (do-cipher-char x (cdr letters) (cdr rot)))))

(define (rot13 x) (do-cipher-char x alphabet nycunorg))

(define (process-stdin)
  (cond ((peek-char) (write-char (rot13 (read-char))) (process-stdin))
        ('t 'f)))

(process-stdin)
