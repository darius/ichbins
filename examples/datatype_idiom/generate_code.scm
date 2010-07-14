"datatype takes a name for the new datatype and a list of constructors."
"Each constructor has a name as well, and a list of types."
"(datatype Expression ((IntConst ((x int)))
		      (Plus ((left Expression) (right Expression)))
		      (Times ((left Expression) (right Expression)))))"
"We want to process a list of datatypes."

(define else 't)

(define linefeed \
 )

(define (write-string x)
  (cond ((null? x) 't)
	((pair? x) (write-char (car x)) (write-string (cdr x)))
	(else (error "write-string is confused"))))

(define (append x y)
  (cond ((null? x) y)
	((pair? x) (cons (car x) (append (cdr x) y)))
	(else (error "append is confused"))))

(define (assert x complaint)
  (cond (x 't)
	(else (error (append "assertion failure: " complaint)))))

(define (endline) (write-char linefeed))

(define (write-line x)
  (write-string x)
  (endline))

(define (error complaint)
  (write-line complaint)
  (abort))

(assert (eq? (not 't) 'f) "not true is false")
(assert (eq? (not 'f) 't) "not false is true")

(define (not x)
  (cond (x 'f)
	(else 't)))

(assert (not (singleton? '())) "null should not be a singleton list")
(assert (singleton? '(x)) "a single x should be a singleton list")
(assert (not (singleton? '(x y))) "a two-element list should not be a singleton list")

(define (singleton? x)
  (cond ((pair? x) (null? (cdr x)))
	(else 'f)))

(assert (string=? "" "") "empty string equals empty string")
(assert (string=? "x" "x") "singleton string equals singleton string")
(assert (not (string=? "x" "")) "singleton string does not equal empty string")
(assert (not (string=? "" "x")) "empty string does not equal singleton string")
(assert (not (string=? "x" "y")) "x does not equal y")
(assert (string=? "xyz" "xyz") "xyz equals xyz")

(define (string? x)
  (cond ((null? x) 't)
	((pair? x) (cond ((char? (car x)) (string? (cdr x)))
			 (else 'f)))
	(else 'f)))

(define (check-string to-check function-name)
  (assert (string? to-check) (append "argument to "
				     (append function-name
					     " should be a string"))))

(define (string=? x y)
  (check-string x "string=?")
  (check-string y "string=?")

  (cond ((null? x) (null? y))
	((null? y) 'f)
	((eq? (car x) (car y)) (string=? (cdr x) (cdr y)))
	(else 'f)))

(define (1st x) (car x))
(define (2nd x) (car (cdr x)))


"Creates a single-element list"
(define (list1 x)
  (cons x '()))

"Creates a two-element list"
(define (list2 x y)
  (cons x (list1 y)))





(define (make-datatype name constructors)
  (cons 'datatypetag (cons name constructors)))

(define (datatype? d) (eq? (car d) 'datatypetag))

(define (check-datatype to-check function-name)
  (assert (datatype? to-check) (append "argument to "
				       (append function-name
					       " should be a datatype"))))

(assert (datatype? (make-datatype 'False '())) "False is a datatype")

(define (datatype-name d)
  (check-datatype d "datatype-name")

  (2nd d))

(define (datatype-visitor-name d)
  (check-datatype d "datatype-visitor-name")

  (append (datatype-name d) "Visitor"))

(assert (string=? (datatype-name (make-datatype 'False '())) "False")
	"The name of False should be False")

(define (datatype-constructors d)
  (check-datatype d "datatype-constructors")

  (cdr (cdr d)))

(define (write-interface-header classname)
  (check-string classname "write-interface-header")
  (write-string "class ")
  (write-string classname)
  (write-string " {")
  (endline)
  (write-string "public:")
  (endline))

(define (write-constructor-declaration classname args)
  (write-line "  // Constructor.")
  (write-string "  ")
  (write-string classname)
  (write-string "(")
  (write-arguments args)
  (write-string ")"))

(define (write-destructor-declaration classname)
  (write-string "  // Destructor.")
  (endline)
  (write-string "  virtual ~")
  (write-string classname)
  (write-string "()"))


(define (write-interface-footer classname)
  (check-string classname "write-interface-footer")

  (endline)
  (write-destructor-declaration classname)
  (write-string " {}")
  (endline)
  (write-string "protected:")
  (endline)

  (write-constructor-declaration classname '())
  (write-string " {}")
  (endline)
  (write-line "};"))

(define (length list)
  (cons (length-helper list "0123456789") '()))
  
(define (length-helper list digits)
  (cond ((null? list) (car digits))
	((pair? list) 
	 (assert (not (null? digits)) "length of list is too big to compute with this brain-dead implementation")
	 (length-helper (cdr list) (cdr digits)))
	('t (assert 'f "length-helper is confused"))))

(define (write-arguments as)
  (cond ((null? as) 't)
	((pair? as)
	 (write-argument (car as))
	 (cond ((pair? (cdr as)) (write-string ", ")))
	 (write-arguments (cdr as)))
	(else (error "write-arguments is confused"))))

(define (write-argument a)
  (assert (string=? (length a) "2") "write-argument takes a two-element list")

  (write-string (2nd a))
  (write-string " ")
  (write-string (car a)))


"TODO: introduce constructors and accessors for methods, not this car/car/cdr stuff."
(define (write-visitor-class-method m)
  (assert (string=? (length m) "2") "length of the argument to write-visitor-class-method should be exactly two")

  (write-string "  virtual void ")
  (write-string (1st m))
  (write-string "(")
  (write-arguments (2nd m))
  (write-string ") { Complain(\"")
  (write-string (1st m))
  (write-string "\"); }")
  (endline))

(define (write-visitor-class-methods ms)
  (cond ((null? ms) 't)
	((pair? ms)
	 (write-visitor-class-method (car ms))
	 (write-visitor-class-methods (cdr ms)))
	(else (error "write-visitor-class-methods is confused"))))

(define (write-visitor-class-helper name methods)
  (write-interface-header name)
  (write-visitor-class-methods methods)
  (write-string "  virtual void Complain(const char* function_name) {}")
  (write-interface-footer name))

(define (write-visitor-class d)
  (check-datatype d "write-visitor-class")

  (write-visitor-class-helper (datatype-visitor-name d) (datatype-constructors d)))

(define (write-datatype-class d)
  (check-datatype d "write-datatype-class")

  (write-interface-header (datatype-name d))

  (write-string "  virtual void Visit(")
  (write-string (datatype-visitor-name d))
  (write-string "&) = 0;")
  (endline)
  
  (write-interface-footer (datatype-name d)))

"Takes a list of datatypes, writes forward declarations for them."
(define (write-forward-declarations ds)
  (cond ((null? ds) 't)
	((pair? ds)
	 (cond ((singleton? ds)
		(write-string "// forward declaration"))
	       (else
		(write-string "// forward declarations")))
	 (endline)
	 (write-forward-declarations-helper ds))
	(else (error "write-forward-declarations is confused"))))

(define (write-forward-declarations-helper ds)
  (cond ((null? ds) 't)
	((pair? ds)
	 (write-forward-declaration (car ds))
	 (write-forward-declarations-helper (cdr ds)))
	(else (error "write-forward-declarations-helper is confused"))))

"Takes a datatype, writes a foward declaration for it."
(define (write-forward-declaration d)
  (check-datatype d "write-forward-declaration")

  (write-string "class ")
  (write-string (datatype-name d))
  (write-string ";")
  (endline))

"Takes a datatype, writes an interface class and subclasses for each."
"Note: visitor classes must be declared (not forward declared) first."
(define (write-datatype d)
  (check-datatype d "write-datatype")

  (write-visitor-class d)
  (endline)
  (write-datatype-class d)
  (endline)
  (write-constructor-classes d))

(define (write-constructor-classes d)
  (check-datatype d "write-constructor-classes")
  
  (write-constructor-classes-helper
   (datatype-name d)
   (datatype-constructors d)))

(define (write-constructor-classes-helper name cs)
  (cond ((null? cs) 't)
	((pair? cs)
	 (write-constructor-class name (car cs))
	 (cond ((pair? (cdr cs)) (endline)))
	 (write-constructor-classes-helper name (cdr cs)))
	(else (error "write-constructor-classes-helper is confused"))))

"TODO: Get a real initializer list working."
(define (write-constructor-class typename c)
  (assert (string=? (length c) "2") "length arg to write-constructor-class should be two")

  (write-string "class ")
  (write-string (1st c))
  (write-string " : public ")
  (write-string typename)
  (write-string " {")
  (endline)

  (write-line "public:")

  (write-constructor-declaration (1st c) (2nd c))

  (write-simple-initializer-list (2nd c))

  (write-string "  {}")
  (endline)

  (write-string "  virtual void Visit(")
  (write-string typename)
  (write-string "Visitor& v) {")
  (endline)

  (write-string "    v.")
  (write-string (1st c))
  (write-string "(")
  (write-unpack-fields (2nd c))
  (write-string ");")
  (endline)

  (write-line "  }")


  (write-line "private:")

  (write-member-variables (2nd c))

  (write-line "};"))

(define (write-unpack-fields fields)
  (cond ((null? fields) 't)
	((pair? fields)
	 (write-string (1st (car fields)))
	 (cond ((pair? (cdr fields)) (write-string ", ")))
	 (write-unpack-fields (cdr fields)))
	(else (error "write-unpack-fields is confused"))))

(define (write-simple-initializer-list fields)
  (cond ((null? fields) (endline))
	((pair? fields)
	 (write-string " :")
	 (endline)
	 (write-simple-initializer-list-helper fields))
	(else (error "write-simple-initializer-list is confused"))))

(define (write-simple-initializer-list-helper fields)
  (cond ((null? fields) 't)
	((pair? fields)
	 (write-string "    ")
	 (write-simple-initializer (car fields))
	 (cond ((pair? (cdr fields)) (write-string ",")))
	 (endline)
	 (write-simple-initializer-list-helper (cdr fields)))
	(else (error "write-simple-initializer-list is confused"))))

(define (write-simple-initializer field)
  (assert (string=? (length field) "2") "write-simple-initializer takes a two-element list")

  (write-string (1st field))
  (write-string "(")
  (write-string (1st field))
  (write-string ")"))

(define (write-member-variables vars)
  (cond ((null? vars) 't)
	((pair? vars)
	 (write-string "  ")
	 (write-variable (car vars))
	 (endline)
	 (write-member-variables (cdr vars)))
	(else (error "write-member-variables is confused"))))

(define (write-variable v)
  (assert (string=? (length v) "2") "write-variable takes a two-element list")

  (write-string (2nd v))
  (write-string " ")
  (write-string (1st v))
  (write-string ";"))

"Takes a list of datatypes"
(define (write-datatypes ds)
  (write-forward-declarations ds)
  (endline)
  (write-datatypes-helper ds))

(define (write-datatypes-helper ds)
  (cond ((null? ds) 't)
	((pair? ds)
	 (write-datatype (car ds))
	 (cond ((pair? (cdr ds)) (endline)))
	 (write-datatypes-helper (cdr ds)))
	(else (error "write-datatypes-helper is confused"))))


(define (read)
  (read-dispatch (skip-blanks (read-char))))

(define (memq? x xs)
  (cond ((null? xs) 'f)
	((eq? x (car xs)) 't)
	(else (memq? x (cdr xs)))))

(define (skip-blanks c)
  (cond ((memq? c whitespace-chars) (skip-blanks (read-char)))
	(else
	 '(write-string "skip-blanks returning: ")
	 '(write-char c)
	 '(endline) 
	 c)))

(define whitespace-chars (cons linefeed " 	"))

(define eof-object '("eof"))

(define (read-dispatch c)
  (cond ((eq? c 'f) eof-object)
	((eq? c \") (read-string (read-char)))
	((eq? c \() (read-list))
	((eq? c \)) (error "Unbalanced parentheses"))
	(else (cons c (read-symbol (peek-char))))))

(define (read-string c)
  (cond ((eq? c 'f) (error "Unterminated string literal"))
	((eq? c \") '())
        ((eq? c \\) (cons (read-char) (read-string (read-char))))
	(else (cons c (read-string (read-char))))))

(define non-symbol-chars "\"\\(')")

(define (read-symbol c)
  (cond ((memq? c whitespace-chars) '())
	((memq? c non-symbol-chars) '())
	(else (read-char) (cons c (read-symbol (peek-char))))))

(define (read-list)
  (read-list-dispatch (skip-blanks (read-char))))

(define (read-list-dispatch c)
  (cond ((eq? c 'f) (error "Unterminated list"))
	((eq? c \)) '())
	(else (cons (read-dispatch c) (read-list)))))

(define (make-list-of-datatypes lds)
  (cond ((null? lds) '())
	((pair? lds)
	 (cons (make-datatype (1st (car lds)) (2nd (car lds)))
	       (make-list-of-datatypes (cdr lds))))
	(else (error "make-list-of-datatypes is confused"))))


'(write-interface-header "Foo")
'(write-interface-footer "Foo")

'(define Expression (make-datatype "Expression" '((IntConst ((x int)))
						 (Plus ((left Expression) (right Expression)))
						 (Times ((left Expression) (right Expression))))))
'(write-visitor-class Expression)

'(write-forward-declarations (list1 Expression))

'(write-visitor-classes (list1 Expression))

'(write-datatype Expression)
'(write-datatype Tree)
'(write-datatype ListOfTrees)
'(write-constructor-classes Expression)

'(define Tree (make-datatype "Tree" '((Leaf ((value int)))
				     (TreeNode ((left Tree*) (right Tree*))))))

'(define ListOfTrees (make-datatype "ListOfTrees" '((Empty ())
						   (Nonempty ((first Tree*) (rest ListOfTrees*))))))


'(write-datatypes (list1 Expression))

'(write-datatypes (list2 Tree ListOfTrees))



"This is the expression that starts everything."
(write-datatypes (make-list-of-datatypes (read)))

