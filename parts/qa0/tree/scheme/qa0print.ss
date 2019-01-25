;; AST tree printer
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "print.ss"
#fload "ast.ss"
;;
;; (provide print-tree)
;;
(define (print-tree ast)
  (define (print-decl decl level)
    (variant-case decl
      [qa0-alias (old-name new-name)
	(print-alias level old-name new-name)]
      [qa0-const (name value)
	(print-const level name value)]
      [qa0-struct (name c-name field-name* field-type* field-c-name*)
	(print-struct level name c-name field-name* field-type*
		      field-c-name*)]
      [qa0-array (name c-name base-name size)
        (print-array level name c-name base-name size)]
      [qa0-proc (attr* name arg-name* arg-type* arg-c-name* arg-c-type* code*)
        (print-proc level attr* name
		    arg-name* arg-type* arg-c-name* arg-c-type* code*)]
      [qa0-verbose (target* data*)
	(print-verbose level target* data*)]
      [qa0-repeat-list (id value* body*)
        (print-repeat-list level id value* body*)]
      [qa0-repeat-range (id low high body*)
        (print-repeat-range level id low high body*)]))
  (define (print-start level)
    (do ([i 0 (+ i 1)])
	[(= i level)]
      (q-print "  ")))
  (define (print-verbose level target* data*)
    (print-start level)
    (q-print "Verbose (~a) cases:~%" (length target*))
    (do ([target* target* (cdr target*)]
	 [data* data* (cdr data*)])
	[(null? target*)]
      (print-start (+ 1 level))
      (q-print "~a: ~s~%" (car target*) (car data*)))
    (print-start level)
    (q-print "end(verbose)~%"))
  (define (print-alias level old-name new-name)
    (print-start level)
    (q-print "Alias of ~a is ~a~%" old-name new-name))
  (define (print-const level name value)
    (print-start level)
    (q-print "Constant ~a = " name)
    (print-input value)
    (q-print "~%"))
  (define (print-struct level name c-name field-name* field-type* field-c-name*)
    (print-start level)
    (q-print "Structure ~a ~s:~%" name c-name)
    (for-each (lambda (name type c-name)
		(print-start (+ level 1))
		(q-print "~a ~a ~s~%" name type c-name))
	      field-name* field-type* field-c-name*))
  (define (print-array level name c-name base-name size)
    (print-start level)
    (q-print "Array ~a ~s is ~a of " name c-name base-name)
    (print-input size)
    (q-print "~%"))
  (define (print-proc level attr* name
		      arg-name* arg-type* arg-c-name* arg-c-type* code*)
    (print-start level)
    (q-print "Procedure ~a attrs " name)
    (print-attr* attr*)
    (q-print "~%")
    (for-each (lambda (n t c-n c-t)
		(print-start (+ level 3))
		(q-print "[~a ~a ~s ~s]~%" n t c-n c-t))
	      arg-name* arg-type* arg-c-name* arg-c-type*)
    (print-code* (+ level 1) code*)
    (print-start level)
    (q-print "Procedure ~a end~%" name))
  (define (print-repeat-list level id value* body*)
    (print-start level)
    (q-print "Repeat ~a in ~s~%" id value*)
    (for-each (lambda (decl) (print-decl (+ level 1) decl)) body*)
    (print-start level)
    (q-print "Repeat ~a end~%" id))
  (define (print-repeat-range level id low high body*)
    (print-start level)
    (q-print "Repeat ~a from " id)
    (print-input low)
    (q-print " to ")
    (print-input high)
    (q-print "~%")
    (for-each (lambda (decl) (print-decl (+ level 1) decl)) body*)
    (print-start level)
    (q-print "Repeat ~a end~%" id))
  (define (print-code* level code*)
    (for-each (lambda (code) (print-code level code)) code*))
  (define (print-code level code)
    (variant-case code
      [qa0-operation (attr* name output* input*)
        (print-operation level attr* name output* input*)]
      [qa0-load (attr* type output addr*)
        (print-load level attr* type output addr*)]
      [qa0-store (attr* type addr* value)
        (print-store level attr* type addr* value)]
      [qa0-loop (attr* var low high code*)
        (print-loop level attr* var low high code*)]
      [qa0-if (var true-code* false-code*)
        (print-if level var true-code* false-code*)]
      [qa0-macro-list (id value* code*)
        (print-macro-list level id value* code*)]
      [qa0-macro-range (id low high code*)
        (print-macro-range level id low high code*)]))
  (define (print-operation level attr* name output* input*)
    (print-start level)
    (q-print "op ")
    (print-attr* attr*)
    (q-print " (")
    (print-output* output*)
    (q-print ") <- ~a (" name)
    (print-input* input*)
    (q-print ")~%"))
  (define (print-load level attr* type output addr*)
    (print-start level)
    (q-print "load ~a " type)
    (print-attr* attr*)
    (q-print " ")
    (print-output output)
    (q-print " <- Mem{")
    (print-input* addr*)
    (q-print "}~%"))
  (define (print-store level attr* type addr* value)
    (print-start level)
    (q-print "store ~a " type)
    (print-attr* attr*)
    (q-print " Mem{")
    (print-input* addr*)
    (q-print "} <- ")
    (print-input value)
    (q-print "~%"))
  (define (print-loop level attr* var low high code*)
    (print-start level)
    (q-print "Loop ")
    (print-attr* attr*)
    (q-print " ~a from " (reg->name var))
    (print-input low)
    (q-print " to ")
    (print-input high)
    (q-print "~%")
    (print-code* (+ level 1) code*)
    (print-start level)
    (q-print "Loop ~a end~%" (reg->name var)))
  (define (print-if level var true-code* false-code*)
    (print-start level)
    (q-print "If ")
    (print-input var)
    (q-print " then~%")
    (print-code* (+ level 1) true-code*)
    (print-start level)
    (q-print "else~%")
    (print-code* (+ level 1) false-code*)
    (print-start level)
    (q-print "fi~%"))
  (define (print-macro-list level id value* body*)
    (print-start level)
    (q-print "Macro ~a in ~s~%" id value*)
    (print-code* (+ level 1) body*)
    (print-start level)
    (q-print "Macro ~a end~%" id))
  (define (print-macro-range level id low high body*)
    (print-start level)
    (q-print "Macro ~a from " id)
    (print-input low)
    (q-print " to ")
    (print-input high)
    (q-print "~%")
    (print-code* (+ level 1) body*)
    (print-start level)
    (q-print "Macro ~a end~%" id))
  (define (print-input* input*) (print-list print-input input*))
  (define (print-output* output*) (print-list print-output output*))
  (define (print-attr* attr*)
    (q-print "(") (print-list print-attr attr*) (q-print ")"))
  (define (print-list print-it l*)
    (if (null? l*) #f
	(begin (print-it (car l*))
	       (do ([l* (cdr l*) (cdr l*)])
		   [(null? l*)]
		 (q-print " ")
		 (print-it (car l*))))))
  (define (write-list l*)
    (if (null? l*) #f
	(begin (q-write (car l*))
	       (do ([l* (cdr l*) (cdr l*)])
		   [(null? l*)]
		 (q-print " ")
		 (q-write (car l*))))))
  (define (print-attr attr)
    (variant-case attr
      [qa0-attr (name value*)
	(if (null? value*) (q-print "~a" name)
	    (begin (q-print "[~a " name)
		   (write-list value*)
		   (q-print "]")))]))
  (define (print-output out)
    (variant-case out
      [reg (name) (q-print "~a" name)]))
  (define (print-input in)
    (variant-case in
      [reg (name) (q-print "[reg ~a]" name)]
      [else (q-print "[const ") (print-cexpr in) (q-print "]")]))
  (define (print-cexpr ce)
    (variant-case ce
      [c-expr-quote (literal) (q-print "'~a" literal)]
      [c-expr-id (id) (q-print "~a" id)]
      [c-expr-number (number) (q-print "~a" number)]
      [c-expr-string (string) (q-print "~s" string)]
      [c-expr-op (name c-expr*) (q-print "(~a " name)
		                (print-list print-cexpr c-expr*)
				(q-print ")")]))
  (variant-case ast
    [qa0-top (decl*) (for-each (lambda (decl) (print-decl decl 0)) decl*)]))

