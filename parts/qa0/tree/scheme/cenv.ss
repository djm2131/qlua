;; Compile Environment operations
;;
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "print.ss"
#fload "format.ss"
#fload "basis.ss"
#fload "ast.ss"
#fload "fmap.ss"
;;
;; (provide ce-empty-env)
;; (provide ce-search)
;; (provide ce-search-x)
;; (provide ce-lookup)
;; (provide ce-lookup-x)
;; (provide ce-resolve-const)
;; (provide ce-bind)
;; (provide ce-bind-x)
;; (provide ce-add-param)
;; (provide ce-add-param*)
;; (provide ce-add-const)
;; (provide ce-add-type)
;; (provide ce-add-array)
;; (provide ce-add-struct)
;; (provide ce-add-alias)
;; (provide ce-add-macro)
;; (provide ce-add-qcd-type)
;; (provide ce-for-each)
;;

(define-syntax ce-extend
  (syntax-rules ()
    [(_ env key val) (extend-fmap env key val)]))
(define-syntax ce-empty-env
  (syntax-rules ()
    [(_) (empty-fmap)]))
(define-syntax ce-search
  (syntax-rules ()
    [(_ env key k-found k-missed) (lookup-fmap env key k-found k-missed)]))
(define-syntax ce-for-each
  (syntax-rules ()
    [(_ env predicate? proc) (fmap-for-each env predicate? proc)]))

(define (ce-search-x env type key k-found k-missed)
  (ce-search env (list type key) k-found k-missed))
(define-syntax ce-lookup
  (syntax-rules ()
    [(_ env key msg arg ...)
     (ce-search env key (lambda (x) x)
                (lambda () (ic-error 'ce-lookup msg arg ...)))]))
(define-syntax ce-lookup-x
  (syntax-rules ()
    [(_ env type key msg arg ...)
     (ce-search env (list type key) (lambda (x) x)
                (lambda ()
                  (ic-error 'ce-lookup-x msg arg ...)))]))
(define-syntax ce-resolve-const
  (syntax-rules ()
    [(_ env key msg arg ...)
     (if (number? key) key
         (ce-lookup-x env 'const key msg arg ...))]))
(define (ce-bind env k v) (ce-extend env k v))
(define (ce-bind-x env t k v) (ce-extend env (list t k) v))
(define (ce-add-param env name value)
  (let* ([env (ce-bind-x env 'type name 'param)]
         [env (ce-bind-x env 'param name value)])
    env))
(define (ce-add-param* env name* value*)
  (if (null? name*) env
      (ce-add-param* (ce-add-param env (car name*) (car value*))
                     (cdr name*) (cdr value*))))
(define (ce-add-const env name value)
  (let ([t (list 'type name)])
    (ce-search env t
               (lambda (v)
                 (s-error "Rebinding ~a to ~a is not allowed, old binding ~a"
                          name value v))
               (lambda ()
                 (let* ([env (ce-bind env t 'const)]
                        [env (ce-bind-x env 'const name value)])
                   env)))))
(define (ce-add-type-2 env name c-name size align elem)
  (let ([t (list 'type name)])
    (ce-search env t
               (lambda (ignore)
                 (s-error "Redefining type ~a is not allowed" name))
               (lambda ()
                 (let* ([env (ce-bind env t 'type)]
                        [env (ce-bind-x env 'size-of name size)]
                        [env (ce-bind-x env 'align-of name align)]
                        [env (ce-bind-x env 'components name '())]
                        [env (ce-bind-x env 'aliased-to name name)]
                        [env (ce-bind-x env 'element-name name elem)]
                        [env (ce-bind-x env 'name-of name c-name)])
                   env)))))
(define (ce-add-type env name c-name size align)
  (ce-add-type-2 env name c-name size align #f))
(define (ce-add-array env name c-name base size)
  (let ([bs (ce-lookup-x env 'size-of base
                         "Size of array base ~a" base)]
        [ba (ce-lookup-x env 'align-of base
                          "Alignment of array base ~a" base)]
        [t (list 'type name)])
    (ce-search env t
               (lambda (ignore)
                 (s-error "Redefining array ~a is not allowed" name))
               (lambda ()
                 (let* ([env (ce-bind env t 'array)]
                        [env (ce-bind-x env 'size-of name (* size bs))]
                        [env (ce-bind-x env 'align-of name ba)]
                        [env (ce-bind-x env 'components name '())]
                        [env (ce-bind-x env 'aliased-to name name)]
                        [env (ce-bind-x env 'element-name name base)]
                        [env (ce-bind-x env 'name-of name c-name)])
                   env)))))
(define (ce-add-struct env name c-name field* type*)
  (let ([t (list 'type name)])
    (ce-search env t
               (lambda (ignore)
                 (s-error "Redefining structure ~a is not allowed" name))
               (lambda ()
                 (let loop ([env env] [f* field*] [t* type*]
                            [size 0] [align 1])
                   (cond
                    [(null? f*)
                     (let* ([env (ce-bind-x env 'type name 'struct)]
                            [env (ce-bind-x env 'size-of name size)]
                            [env (ce-bind-x env 'align-of name align)]
                            [env (ce-bind-x env 'components name field*)]
                            [env (ce-bind-x env 'aliased-to name name)]
                            [env (ce-bind-x env 'element-name name #f)]
                            [env (ce-bind-x env 'name-of name c-name)])
                       env)]
                    [else
                     (let* ([f (car f*)] [t (car t*)]
                            [a-f (ce-lookup-x env 'align-of t
                                              "Alignment of ~a.~a" name f)]
                            [s-f (ce-lookup-x env 'size-of t
                                              "Size of field ~a.~a" name f)]
                            [start (* a-f (quotient (+ size a-f -1) a-f))]
                            [align (max a-f align)]
                            [env (ce-bind env (list 'offset-of name f) start)])
                       (loop env (cdr f*) (cdr t*) (+ start s-f) align))]))))))
(define (ce-add-alias env new old)
 (let ([t (ce-lookup-x env 'type old "type of ~a" old)])
    (case t
      [(const) (let* ([v (ce-lookup-x env 'const old "value of ~a" old)]
                      [env (ce-bind-x env 'type new t)]
                      [env (ce-bind-x env 'const new v)])
                 env)]
      [(type array struct)
       (let* ([c (ce-lookup-x env 'components old
                              "components of ~a" old)]
              [s (ce-lookup-x env 'size-of old "size of ~a" old)]
              [a (ce-lookup-x env 'align-of old "align of ~a" old)]
              [x (ce-lookup-x env 'name-of old "C name of ~a" old)]
              [e (ce-lookup-x env 'element-name old "element name of ~a" old)]
              [tn (ce-lookup-x env 'aliased-to old "True name of ~a" old)]
              [env (ce-bind-x env 'type new t)]
              [env (ce-bind-x env 'size-of new s)]
              [env (ce-bind-x env 'align-of new a)]
              [env (ce-bind-x env 'components new c)]
              [env (ce-bind-x env 'aliased-to new tn)]
              [env (ce-bind-x env 'element-name new e)]
              [env (ce-bind-x env 'name-of new x)])
         (let loop ([c c] [env env])
           (cond
            [(null? c) env]
            [else (let ([o (ce-lookup env (list 'offset-of old (car c))
                                      "offset of ~a in ~a"
                                      (car c) old)])
                    (loop (cdr c)
                          (ce-bind env (list 'offset-of new (car c))
                                   o)))])))]
      [else (s-error "Unexpected type for aliasing: ~a" t)])))
(define (ce-add-macro env name value)
  (let* ([env (ce-bind-x env 'type name 'macro)]
         [env (ce-bind-x env 'macro name value)])
    env))
(define (ce-add-qcd-type env name c-name a-dim b-dim elem)
  (let* ([c-type (case elem
                   [(complex-float complex-double) elem]
                   [else (ic-error 'ce-add-qcd-type "Bad elem type ~s" elem)])]
         [c-size (ce-lookup-x env 'size-of c-type "(size-of <elem>)")]
         [c-align (ce-lookup-x env 'align-of c-type "(align-of <elem>)")]
         [a-size (ce-resolve-const env a-dim "(const ~a)" a-dim)]
         [b-size (ce-resolve-const env b-dim "(const ~a)" b-dim)])
    (ce-add-type-2 env name c-name (* a-size b-size c-size) c-align elem)))
(define (ce-add-qcd-matrix-type env name c-name a-dim n elem)
  (let* ([c-type (case elem
                   [(complex-float complex-double) elem]
                   [else (ic-error 'ce-add-qcd-type "Bad elem type ~s" elem)])]
         [c-size (ce-lookup-x env 'size-of c-type "(size-of <elem>)")]
         [c-align (ce-lookup-x env 'align-of c-type "(align-of <elem>)")]
         [a-size (ce-resolve-const env a-dim "(const ~a)" a-dim)])
    (ce-add-type-2 env name c-name (* a-size a-size n c-size) c-align elem)))
