;; Backend generator
#fload "sfc.sf"
#fload "error.ss"
#fload "cenv.ss"
;;
;; (provide complex->back-end)
;; (provide emit-back-end)
;; (provide be-out-type*)
;; (provide be-load-type)
;; (provide C-collect-args)
;; (provide machine-*-*)
;;
(define (complex->back-end ast env)
  ((ce-lookup env 'back-end "Back end transformer not found") ast env))
(define (emit-back-end ast env)
  ((ce-lookup env 'be-emit "Code emitter not found") ast env))
(define (be-out-type* env name)
  (ce-lookup-x env 'be-op-type* name
               "Unknown operation ~a in be-op-type*" name))
(define (be-load-type env load-op)
  (ce-lookup-x env 'be-load-type load-op
               "Unknown load operation ~a in be-load-type" load-op))
(define (C-collect-args arg-name* arg-type* new-var env)
  (cond
   [(null? arg-name*) env]
   [else (let* ([env (ce-bind-x env 'back-end (car arg-name*)
                                (list (new-var) (car arg-type*)))])
           (C-collect-args (cdr arg-name*) (cdr arg-type*) new-var env))]))
(define (machine-*-* int-size int-align pointer-size pointer-align env op* load*)
  (let* ([env (ce-add-type env 'int            "size_t"           int-size int-align)]
         [env (ce-add-type env 'pointer        "unsigned char *"  pointer-size pointer-align)]
         [env (ce-add-type env 'float          "float"            4  4)]
         [env (ce-add-type env 'double         "double"           8  8)]
         [env (ce-add-type env 'vector-float   "vector float"     8  8)]
         [env (ce-add-type env 'vector-double  "vector double"   16 16)]
         [env (ce-add-type env 'complex-float  "float _Complex"   8  8)]
         [env (ce-add-type env 'complex-double "double _Complex" 16 16)]
         [Nc (or (string->number (or (getenv "Nc") "3")) 3)]
         [env (ce-add-const env '*colors*            Nc)]
         [env (ce-add-const env '*dim*               4)]
         [env (ce-add-const env '*clovers*           (* 2 Nc))]
         [env (ce-add-const env '*fermion-dim*       4)]
         [env (ce-add-const env '*projected-fermion-dim*  2)])
    (let loop ([env env] [op* op*])
      (cond
       [(null? op*)
        (let loop ([env env] [load* load*])
          (cond
           [(null? load*) env]
           [else (loop (ce-bind-x env 'be-load-type (caar load*)
                                  (cadar load*))
                       (cdr load*))]))]
       [else (loop (ce-bind-x env 'be-op-type* (caar op*) (cdar op*))
                   (cdr op*))]))))
