;; AST
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "format.ss"
;;
;; (provide gen-reg)
;; (provide walk-code*)
;; provide all variants
;; 
(define-variant qa0-top (decl*))
(define-variant qa0-verbose (target* data*))
(define-variant qa0-name-prefix (name))
(define-variant qa0-name-infix (name))
(define-variant qa0-name-suffix (name))
(define-variant qa0-alias (old-name new-name))
(define-variant qa0-const (name value))
(define-variant qa0-struct (name c-name
                                 field-name* field-type* field-c-name*))
(define-variant qa0-array (name c-name base-name size))
(define-variant qa0-proc (attr* name
                                arg-name* arg-type* arg-c-name* arg-c-type*
                                code*))
(define-variant qa0-repeat-list (id value* body*))
(define-variant qa0-repeat-range (id low high body*))
(define-variant qa0-operation (attr* name output* input*))
(define-variant qa0-load (attr* type output addr*))
(define-variant qa0-store (attr* type addr* value))
(define-variant qa0-loop (attr* var low high code*))
(define-variant qa0-if (var true-code* false-code*))
(define-variant qa0-macro-def (id arg* code*))
(define-variant qa0-macro-call (id arg*))
(define-variant qa0-macro-list (id value* code*))
(define-variant qa0-macro-range (id low high code*))
(define-variant qa0-attr (name value*))
(define-variant c-expr-macro (name))
(define-variant c-expr-id (id))
(define-variant c-expr-quote (literal))
(define-variant c-expr-number (number))
(define-variant c-expr-string (string))
(define-variant c-expr-op (name c-expr*))
(define-variant reg (name))
(define (walk-code* code* p-op p-load p-store p-loop p-if arg)
  (define (do-walk* code* arg)
    (cond
        [(null? code*) arg]
        [else (do-walk* (cdr code*) (walk-code (car code*) arg))]))
  (define (walk-code code arg)
    (variant-case code
      [qa0-operation (name attr* output* input*)
       (p-op arg name attr* output* input*)]
      [qa0-load (type attr* output addr*)
        (p-load arg type attr* output addr*)]
      [qa0-store (type attr* addr* value)
        (p-store arg type attr* addr* value)]
      [qa0-loop (attr* var low high code*)
        (do-walk* code* (p-loop arg attr* var low high))]
      [qa0-if (var true-code* false-code*)
        (do-walk* true-code* (do-walk* false-code* (p-if arg var)))]))
  (do-walk* code* arg))

(define (gen-reg* prefix r*)
  (let loop ([s (q-fmt "~a" prefix)] [r* r*])
    (cond
     [(null? r*) (string->symbol s)]
     [else (loop (q-fmt "~a~a" s (car r*)) (cdr r*))])))
  
(define-syntax gen-reg
  (syntax-rules ()
    [(_ prefix r ...) (gen-reg* prefix (list r ...))]))
