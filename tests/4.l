(to (string? x)
  (cond ((null? x) 't)
        ((char? x) 'f)
        ((char? (car x)) (string? (cdr x)))
        ('t 'f)))

(to (try x)
  (write-char (cond ((string? x) \t) ('t \f)))
  (write-char \
))

(try '())
(try '"hell")
(try '(\h \e "ll"))
