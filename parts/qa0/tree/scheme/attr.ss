;; Attribute handling
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "ast.ss"
#fload "cenv.ss"
;;
;; (provide attr-search)
;; (provide attr-lookup)
;;
(define (attr-search attr* key found missed)
  (let loop ([attr* attr*])
    (cond
     [(null? attr*) (missed)]
     [else (variant-case (car attr*)
	     [qa0-attr (name value*) (if (eq? key name) (found value*)
					 (loop (cdr attr*)))])])))
(define-syntax attr-lookup
  (syntax-rules ()
    [(_ attr* key msg arg ...)
     (attr-search attr* key (lambda (x) x)
		  (lambda () (ic-error 'attr-lookup msg arg ...)))]))

