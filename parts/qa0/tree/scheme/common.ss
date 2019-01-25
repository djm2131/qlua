;; Macros and generic helper functions
;; 
;; (provide dbg)
;; (provide dbg-env)
;; (provide assert)
;; (provide variant-type )
;; (provide-macro define-variant)  ;; require syntax-case
;; (provide-macro variant-case)    ;; require syntax-case
;; (provide-macro let-values*)
;;
(define dbg-port (current-output-port))
(define *verbose?* #f)
(define (dbg . msg)
  (q-display "D:" dbg-port)
  (for-each (lambda (x) (q-display " " dbg-port) (q-write x dbg-port)) msg)
  (newline dbg-port)
  #f)
(define (dbg-env msg env)
  (q-display (format "E: ~a~%" msg) dbg-port)
  (for-each (lambda (k&v)
	      (q-display (format "  ~a: ~a~%" (car k&v) (cdr k&v)) dbg-port))
	    env))
(define-syntax assert
  (lambda (x)
    (syntax-case x ()
      [(_ test fmt arg ...)
       (syntax (if (not test) (ic-error 'assert (string-append "FAILED: " fmt)
				     arg ...)))])))
(define-syntax define-variant
  (lambda (x)
    (define (mk-name template-id . args)
      (datum->syntax-object template-id
			    (string->symbol (apply string-append
			      (map (lambda (x)
				     (cond
				      [(string? x) x]
				      [(symbol? x) (symbol->string x)]
				      [else (symbol->string
					     (syntax-object->datum x))]))
				   args)))))
    (syntax-case x ()
      [(_ name (field ...))
       (with-syntax
	 ([constructor (mk-name (syntax name) 'make- (syntax name))]
	  [predicate (mk-name (syntax name) (syntax name) "?")]
	  [(reader ...) (map (lambda (fld)
			       (mk-name (syntax name) (syntax name) "->" fld))
			     (syntax->list (syntax (field ...))))]
	  [(writer ...) (map (lambda (fld)
			       (mk-name (syntax name)
					"set-" (syntax name) "-" fld "!"))
			     (syntax->list (syntax (field ...))))]
	  [count (length (syntax->list (syntax (name field ...))))])
	 (with-syntax
	   ([(index ...) (let f ([i 1])
			   (if (= i (syntax-object->datum (syntax count))) '()
			       (cons i (f (+ 1 i)))))])
	   (syntax
	    (begin
	      (define constructor (lambda (field ...)
				    (vector 'name field ...)))
	      (define predicate (lambda (object)
				  (and (vector? object)
				       (= (vector-length object) count)
				       (eq? (vector-ref object 0) 'name))))
	      (define reader (lambda (object) (vector-ref object index)))
	      ...
	      (define writer (lambda (object value)
			       (vector-set! object index value)))
	      ...))))])))
(define variant-type (lambda (variant) (vector-ref variant 0)))
(define-syntax variant-case
  (lambda (x)
    (define (mk-name template-id . args)
      (datum->syntax-object template-id
	(string->symbol (apply string-append
			       (map (lambda (x)
				      (cond
				       [(string? x) x]
				       [(symbol? x) (symbol->string x)]
				       [else (symbol->string
					      (syntax-object->datum x))]))
				    args)))))
    (syntax-case x (else)
      [(_ exp clause ...) (not (identifier? (syntax exp)))
       (syntax (let ([var exp]) (_ var clause ...)))]
      [(_ var) (syntax (ic-error 'variant-case "no clause matches ~a" var))]
      [(_ var (else exp exp1 ...)) (syntax (begin exp exp1 ...))]
      [(_ var (name [(fname field) ...] exp1 exp2 ...) clause ...)
       (with-syntax
	([predicate (mk-name (syntax name) (syntax name) "?")]
	 [(reader ...) (map (lambda (fld)
			      (mk-name (syntax name) (syntax name) "->" fld))
			    (syntax->list (syntax (field ...))))])
	(syntax (if (predicate var)
		    (let ([fname (reader var)] ...) exp1 exp2 ...)
		    (_ var clause ...))))]
      [(_ var (name (field ...) exp1 exp2 ...) clause ...)
       (syntax (_ var (name ([field field] ...) exp1 exp2 ...)
		  clause ...))])))
(define-syntax let-values*
  (lambda (x)
    (syntax-case x ()
      [(_ () e ...) (syntax (let () e ...))]
      [(_ ([(n ...) i ...]) e ...)
       (syntax (call-with-values (lambda () (let () i ...))
		 (lambda (n ...) (let () e ...))))]
      [(_ ([n i ...]) e ...) (syntax (let ([n (let () i ...)])
				       (let () e ...)))]
      [(_ (b0 b1 ...) e ...) (syntax (_ (b0) (_ (b1 ...) e ...)))])))

(define-syntax write-string
  (syntax-rules ()
    [(_ s) (write-string s (current-output-port))]
    [(_ s p) (display s p)]))

(define (display-fixnum x p) (display x p))
(define (display-flonum x p) (display x p))
(define (fl<? a b) (< a b))