;; Constant folding
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "basis.ss"
#fload "ast.ss"
#fload "parser.ss"
#fload "cenv.ss"
;;
;; (provide fold-constants/env)
;;
(define (fold-constants/env ast machine real)
  (define fresh-param-reg
    (let ([*param-count* 0])
      (lambda ()
        (let ([n *param-count*])
          (set! *param-count* (+ n 1))
          (gen-reg 'p n)))))
  (define (ce-start-env machine real)
    (let* ([env (ce-empty-env)]
           [env (machine env)]
           [env (ce-bind env 'proc-prefix "qop_")]
           [env (ce-bind env 'proc-infix "mdwf")]
           [env (ce-bind env 'proc-suffix "")]
           [env (ce-add-qcd-type env 'Fermion-double
                                 "struct FermionD" '*colors*
                                 '*fermion-dim* 'complex-double)]
           [env (ce-add-qcd-type env 'Fermion-float
                                 "struct FermionF" '*colors*
                                 '*fermion-dim* 'complex-float)]
           [env (ce-add-qcd-type env 'Staggered-Fermion-double
                                 "struct StaggeredFermionD" '*colors*
                                 1 'complex-double)]
           [env (ce-add-qcd-type env 'Staggered-Fermion-float
                                 "struct StaggeredFermionF" '*colors*
                                 1 'complex-float)]
           [env (ce-add-qcd-type env 'Projected-Fermion-double
                                 "struct ProjectedFermionD" '*colors* 
                                 '*projected-fermion-dim* 'complex-double)]
           [env (ce-add-qcd-type env 'Projected-Fermion-float
                                 "struct ProjectedFermionF" '*colors*
                                 '*projected-fermion-dim* 'complex-float)]
           [env (ce-add-qcd-matrix-type env 'SU-n-double "struct SUnD"
                                        '*colors* 1 'complex-double)]
           [env (ce-add-qcd-matrix-type env 'SU-n-float "struct SUnF"
                                        '*colors* 1 'complex-float)]
           [env (ce-add-qcd-matrix-type env 'Clover-double "struct CloverD"
                                        '*clovers* 2 'complex-double)]
           [env (ce-add-qcd-matrix-type env 'Clover-float "struct CloverF"
                                        '*clovers* 2 'complex-float)]
           [env
            (case real
              [(double)
               (let* ([env (ce-bind env 'prec-letter  "d")]
                      [env (ce-add-alias env 'REAL 'double)]
                      [env (ce-add-alias env 'COMPLEX 'complex-double)]
                      [env (ce-add-alias env 'VECTOR 'vector-double)]
                      [env (ce-add-alias env 'Fermion 'Fermion-double)]
                      [env (ce-add-alias env 'Staggered-Fermion
                                         'Staggered-Fermion-double)]
                      [env (ce-add-alias env 'Projected-Fermion
                                         'Projected-Fermion-double)]
                      [env (ce-add-alias env 'SU-n 'SU-n-double)]
                      [env (ce-add-alias env 'Clover 'Clover-double)])
                 env)]
              [(float)
               (let* ([env (ce-bind env 'prec-letter  "f")]
                      [env (ce-add-alias env 'REAL 'float)]
                      [env (ce-add-alias env 'COMPLEX 'complex-float)]
                      [env (ce-add-alias env 'VECTOR 'vector-float)]
                      [env (ce-add-alias env 'Fermion 'Fermion-float)]
                      [env (ce-add-alias env 'Staggered-Fermion
                                         'Staggered-Fermion-float)]
                      [env (ce-add-alias env 'Projected-Fermion
                                         'Projected-Fermion-float)]
                      [env (ce-add-alias env 'SU-n 'SU-n-float)]
                      [env (ce-add-alias env 'Clover 'Clover-float)])
                 env)]
              [else (ic-error 'cfold "Bad value of REAL: ~a" real)])]
           [env (ce-add-const env '*mdwf-start-sum-dimension*
                              mdwf-start-sum-dimension)]
           [env (ce-add-const env '*mdwf-start-sum-direction*
                              mdwf-start-sum-direction)])
      (let loop ([env env] [p* mdwf-basis])
        (cond
         [(null? p*) env]
         [else (loop (ce-bind env (caar p*) (cdar p*)) (cdr p*))]))))
  (define (cf-decl ast out* env)
    (variant-case ast
      [qa0-alias (old-name new-name) (cf-alias old-name new-name out* env)]
      [qa0-const (name value) (cf-const name value out* env)]
      [qa0-struct (name c-name field-name* field-type* field-c-name*)
        (cf-struct name c-name
                   field-name* field-type* field-c-name* out* env)]
      [qa0-array (name c-name base-name size)
        (cf-array name c-name base-name size out* env)]
      [qa0-proc (attr* name
                 arg-name* arg-type* arg-c-name* arg-c-type* code*)
        (cf-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type*
                 code* out* env)]
      [qa0-repeat-list (id value* body*)
        (cf-repeat-list id value* body* out* env)]
      [qa0-repeat-range (id low high body*)
        (cf-repeat-range id low high body* out* env)]
      [qa0-macro-def (id)
        (cf-macro-def ast id out* env)]
      [qa0-name-prefix (name) (cf-name-prefix ast name out* env)]
      [qa0-name-infix (name) (cf-name-infix ast name out* env)]
      [qa0-name-suffix (name) (cf-name-suffix ast name out* env)]
      [qa0-verbose () (cf-verbose ast out* env)]))
  (define (cf-macro-def ast id out* env)
    (values out* (ce-add-macro env id ast)))
  (define (cf-name-prefix ast name out* env)
    (values out* (ce-bind env 'proc-prefix name)))
  (define (cf-name-infix ast name out* env)
    (values out* (ce-bind env 'proc-infix name)))
  (define (cf-name-suffix ast name out* env)
    (values out* (ce-bind env 'proc-suffix name)))
  (define (cf-alias old new out* env)
    (values out* (ce-add-alias env new old)))
  (define (cf-const name value out* env)
    (let ([v (cf-eval-const value env)])
      (values out* (ce-add-const env name v))))
  (define (cf-struct name c-name
                     field-name* field-type* field-c-name* out* env)
    (let ([name (cf-eval-param name env)]
          [c-name (cf-eval-param c-name env)]
          [field-name* (cf-eval-param* field-name* env)]
          [field-type* (cf-eval-param* field-type* env)]
          [field-c-name* (cf-eval-param* field-c-name* env)])
      (values (cons (make-qa0-struct name c-name
                                     field-name* field-type* field-c-name*)
                    out*)
              (ce-add-struct env name c-name field-name* field-type*))))
  (define (cf-array name c-name base size out* env)
    (let ([size (cf-eval-const size env)]
          [name (cf-eval-param name env)]
          [c-name (cf-eval-param c-name env)]
          [base (cf-eval-param base env)])
      (values (cons (make-qa0-array name c-name base (cf-rewrap size)) out*)
              (ce-add-array env name c-name base size))))
  (define (cf-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type*
                   code* out* env)
    (values (cons (make-qa0-proc (cf-attr* attr* env)
                                 (cf-eval-param name env)
                                 (cf-eval-param* arg-name* env)
                                 (cf-eval-param* arg-type* env)
                                 (cf-eval-param* arg-c-name* env)
                                 (cf-eval-param* arg-c-type* env)
                                 (cf-block code* env))
                  out*)
            env))
  (define (cf-repeat-list id value* body* out* env)
    (let loop-values ([value* (cf-eval-param* value* env)] [out* out*])
      (cond
       [(null? value*) (values out* env)]
       [else (let ([env-x (ce-add-param env id (car value*))])
               (let loop-body ([body* body*] [out* out*])
                 (cond
                  [(null? body*) (loop-values (cdr value*) out*)]
                  [else (loop-body (cdr body*)
                                   (cf-repeat (car body*)
                                              out* env-x))])))])))
  (define (cf-repeat-range id low high body* out* env)
    (let ([low (cf-eval-const low env)]
          [high (cf-eval-const high env)])
      (let loop-values ([i low] [out* out*])
        (cond
         [(>= i high) (values out* env)]
         [else (let ([env-x (ce-add-param env id i)])
                 (let loop-body ([body* body*] [out* out*])
                   (cond
                    [(null? body*) (loop-values (+ i 1) out*)]
                    [else (loop-body (cdr body*)
                                     (cf-repeat (car body*)
                                                out* env-x))])))]))))
  (define (cf-repeat ast out* env)
    (let-values* ([(out* env)
                     (variant-case ast
                       [qa0-proc (attr* name
                                  arg-name* arg-type*
                                  arg-c-name* arg-c-type*
                                  code*)
                         (cf-proc attr* name
                                  arg-name* arg-type*
                                  arg-c-name* arg-c-type*
                                  code* out* env)]
                       [qa0-repeat-list (id value* body*)
                         (cf-repeat-list id value* body* out* env)]
                       [qa0-repeat-range (id low high body*)
                         (cf-repeat-range id low high body* out* env)])])
      out*))
  (define (cf-verbose ast out* env)
    (values (cons ast out*) env))
  (define (cf-attr attr env)
    (variant-case attr
      [qa0-attr (name value*)
        (make-qa0-attr (cf-eval-param name env)
                       (cf-eval-param* value* env))]))
  (define (cf-attr* attr* env)
    (map (lambda (attr) (cf-attr attr env)) attr*))
  (define (cf-block code* env)
    (reverse (cf-code* code* '() env)))
  (define (cf-code* code* out* env)
    (let loop ([code* code*] [out* out*])
      (cond
       [(null? code*) out*]
       [else (loop (cdr code*) (cf-code (car code*) out* env))])))
  (define (cf-code code out* env)
    (variant-case code
      [qa0-operation (attr* name output* input*)
        (cf-operation attr* name output* input* out* env)]
      [qa0-load (attr* type output addr*)
        (cf-load attr* type output addr* out* env)]
      [qa0-store (attr* type addr* value)
        (cf-store attr* type addr* value out* env)]
      [qa0-loop (attr* var low high code*)
        (cf-loop attr* var low high code* out* env)]
      [qa0-if (var true-code* false-code*)
        (cf-if var true-code* false-code* out* env)]
      [qa0-macro-call (id arg*)
        (cf-macro-call id arg* out* env)]
      [qa0-macro-list (id value* code*)
        (cf-macro-list id value* code* out* env)]
      [qa0-macro-range (id low high code*)
        (cf-macro-range id low high code* out* env)]))
  (define (cf-if var true* false* out* env)
    (let ([var (cf-input var env)])
      (variant-case var
        [c-expr-number (number) (if (zero? number) (cf-code* false* out* env)
                                    (cf-code* true* out* env))]
        [reg (name) (cons (make-qa0-if var (cf-block true* env)
                                       (cf-block false* env))
                          out*)])))
  (define (cf-operation attr* name output* input* out* env)
    (cons (make-qa0-operation (cf-attr* attr* env)
                              (cf-eval-param name env)
                              (cf-output* output* env)
                              (cf-input* input* env))
          out*))
  (define (cf-load attr* type output addr* out* env)
    (cons (make-qa0-load (cf-attr* attr* env)
                         (cf-type type env)
                         (cf-output output env)
                         (cf-addr* addr* env))
          out*))
  (define (cf-store attr* type addr* value out* env)
    (cons (make-qa0-store (cf-attr* attr* env)
                          (cf-type type env)
                          (cf-addr* addr* env)
                          (cf-input value env))
          out*))
  (define (cf-loop attr* var low high code* out* env)
    (cons (make-qa0-loop (cf-attr* attr* env)
                         (make-reg (cf-eval-param (reg->name var) env))
                         (cf-input low env)
                         (cf-input high env)
                         (cf-block code* env))
          out*))
  (define (cf-macro-call id p* out* env)
    (let ([vv (cf-eval-mname id env)])
      (variant-case (ce-lookup-x env 'macro vv "Macro call of ~a (~a)" vv id)
        [qa0-macro-def (arg* code*)
          (if (not (= (length arg*) (length p*)))
              (s-error "Macro call ~a expects ~a arguments"
                       (list* 'macro id p*) (length arg*)))
          (let-values* ([(env v*) (ne-start* env arg* p* env '())]
                        [(env v*) (ne-mcode* env code* v*)])
            (let loop ([out* out*] [code* code*])
              (cond
               [(null? code*) out*]
               [else (loop (cf-code (car code*) out* env)
                           (cdr code*))])))])))
  (define (ne-start* env a* p* ine v*)
    (cond
      [(null? p*) (values env v*)]
      [else (let-values* ([(env v*) (ne-start env (car a*) (car p*) ine v*)])
              (ne-start* env (cdr a*) (cdr p*) ine v*))]))
  (define (ne-start env a p ine v*)
    (let ([p (cf-param p ine)])
      (variant-case p
        [reg (name) (let ([a (user-reg a)])
                      (values (ce-add-param env a name) (cons a v*)))]
        [else (values (ce-add-param env a p) (cons a v*))])))
  (define (ne-mcode* env code* v*)
    (cond
     [(null? code*) (values env v*)]
     [else (let-values* ([(env v*) (ne-mcode env (car code*) v*)])
             (ne-mcode* env (cdr code*) v*))]))
  (define (ne-mcode env code v*)
    (variant-case code
      [qa0-operation (output* input*)
        (let-values* ([(env v*) (ne-input* env input* v*)])
          (ne-output* env output* v*))]
      [qa0-load (output addr*)
        (let-values* ([(env v*) (ne-addr* env addr* v*)])
          (ne-output env output v*))]
      [qa0-store (addr* value)
        (let-values* ([(env v*) (ne-input env value v*)])
          (ne-addr* env addr* v*))]
      [qa0-loop (var low high code*)
        (let-values* ([(env v*) (ne-output env var v*)]
                      [(env v*) (ne-input env low v*)]
                      [(env v*) (ne-input env high v*)])
          (ne-mcode* env code* v*))]
      [qa0-if (var true-code* false-code*)
        (let-values* ([(env v*) (ne-output env var v*)]
                      [(env v*) (ne-mcode* env true-code* v*)])
          (ne-mcode* env false-code* v*))]
      [qa0-macro-call (arg*) (ne-marg* env arg* v*)]
      [qa0-macro-list (id code*)
        (let-values* ([(env v*) (ne-mcode* env code* v*)])
          (ne-mvar env id v*))]
      [qa0-macro-range (id code*)
        (let-values* ([(env v*) (ne-mcode* env code* v*)])
          (ne-mvar env id v*))]))
  (define (ne-input* env input* v*)
    (cond
     [(null? input*) (values env v*)]
     [else (let-values* ([(env v*) (ne-input env (car input*) v*)])
             (ne-input* env (cdr input*) v*))]))
  (define (ne-input env input v*)
    (variant-case input
      [reg (name) (if (memq name v*) (values env v*)
                      (values (ne-add-binding env name) (cons name v*)))]
      [else (values env v*)]))
  (define (ne-output* env output* v*) (ne-input* env output* v*))
  (define (ne-output env output v*) (ne-input env output v*))
  (define (ne-addr* env addr* v*) (ne-input* env addr* v*))
  (define (ne-marg* env arg* v*) (ne-input* env arg* v*))
  (define (ne-mvar env var v*) (ne-input env var v*))
  (define (ne-add-binding env name)
    (ce-add-param env name (fresh-param-reg)))
  (define (cf-macro-list id value* code* out* env)
    (let loop ([value* (cf-eval-param* value* env)] [out* out*])
      (cond
       [(null? value*) out*]
       [else (let ([env-x (ce-add-param env id (car value*))])
               (loop (cdr value*) (cf-code* code* out* env-x)))])))
  (define (cf-macro-range id low high code* out* env)
    (let ([low (cf-eval-const low env)]
          [high (cf-eval-const high env)])
      (let loop ([i low] [out* out*])
        (cond
         [(>= i high) out*]
         [else (loop (+ i 1) (cf-code* code* out*
                                       (ce-add-param env id i)))]))))
  (define (cf-input* input* env)
    (map (lambda (in) (cf-input in env)) input*))
  (define (cf-input input env)
    (variant-case input
      [reg (name) (make-reg (cf-eval-param name env))]
      [else (cf-rewrap (cf-eval-const input env))]))
  (define (cf-param input env)
    (variant-case input
      [reg (name) (make-reg (cf-eval-param name env))]
      [c-expr-macro (name)
        (let ([t (ce-lookup-x env 'type name "Macro binding of ~a" name)])
          (case t
            [(macro) name]
            [(param) (ce-lookup-x env 'param name "Macro arg binding of ~a"
                                  name)]
            [else (ic-error 'cf-param "??? name ~a, type ~a" name t)]))]
      [else (cf-reparam (cf-eval-const input env))]))
  (define (cf-addr* addr* env)
    (cf-input* addr* env))
  (define (cf-output* output* env)
    (map (lambda (out) (cf-output out env)) output*))
  (define (cf-output output env)
    (variant-case output
      [reg (name) (make-reg (cf-eval-param name env))]))
  (define (cf-rewrap c)
    (cond
     [(number? c) (make-c-expr-number c)]
     [(string? c) (make-c-expr-string c)]
     [(symbol? c) (make-c-expr-quote c)]
     [else (ic-error 'cfold "Unexpected computed constant is ~a" c)]))
  (define (cf-reparam c)
    (cond
     [(number? c) c]
     [(string? c) c]
     [(symbol? c) c]
     [else (ic-error 'cf-reparam "c=~a" c)]))
  (define (cf-type type env)
    (let ([type (cf-eval-param type env)])
      (ce-search-x env 'aliased-to type (lambda (x) x) (lambda () type))))
  (define (cf-eval-const const env)
    (variant-case const
      [c-expr-quote (literal) literal]
      [c-expr-op (name c-expr*) (cx-const-op name c-expr* env)]
      [c-expr-string (string) string]
      [c-expr-number (number) number]
      [c-expr-id (id) (let ([t (ce-lookup-x env 'type id "Type of ~a" id)]
                            [v (cf-eval-id id env)])
                        (cond
                          [(number? v) v]
                          [(string? v) v]
                          [else (ce-search-x env 'const v
                                             (lambda (x) x)
                                             (lambda () v))]))]))
  (define (cx-const-op name expr* env)
    (define (check-id-arg v)
      (variant-case v
        [c-expr-id () #t]
        [else (s-error "Expecting id, found ~a" v)]))
    (define (check-id-arg* arg* len)
      (if (not (and (list? arg*)
                    (= (length arg*) len)))
          (s-error "Expecting a list of ~a arguments, found ~a" len arg*)
          (let loop ([arg* arg*])
            (cond
             [(null? arg*) #t]
             [else (check-id-arg (car arg*))
                   (loop (cdr arg*))]))))
    (define (get-id v env)  ; for types and fields
      (variant-case v
        [c-expr-id (id) (cf-eval-param id env)]))
    (define (compute-arith* v-0 op1 op+)
      (let ([arg* (map (lambda (arg) (cf-eval-const arg env)) expr*)])
        (cond
         [(zero? (length arg*)) v-0]
         [(= 1 (length arg*)) (op1 (car arg*))]
         [else (let loop ([arg* (cddr arg*)]
                          [v (op+ (car arg*) (cadr arg*))])
                 (cond
                  [(null? arg*) v]
                  [else (loop (cdr arg*) (op+ v (car arg*)))]))])))
    (define (compute-arith2 op)
      (let ([arg* (map (lambda (arg) (cf-eval-const arg env)) expr*)])
        (if (not (= (length arg*) 2))
            (s-error "Expecting a list of 2, found ~a" arg*))
        (map check-number-arg arg*)
        (op (car arg*) (cadr arg*))))
    (define (check-number-arg arg)
      (if (not (number? arg))
          (s-error "A number is expected, found ~a" arg)))
    (define (compute-equal)
      (or (null? expr*)
          (let loop ([v (cf-eval-const (car expr*) env)] [arg* (cdr expr*)])
            (cond
             [(null? arg*) 1]
             [(equal? v (cf-eval-const (car arg*) env)) (loop v (cdr arg*))]
             [else 0]))))
    (define (compute-shift)
      (cond
       [(not (= (length expr*) 2))
        (s-error "Bad number of args for shift")]
       [else (let ([v (cf-eval-const (car expr*) env)]
                   [s (cf-eval-const (cadr expr*) env)])
               (check-number-arg v)
               (check-number-arg s)
               (let loop ([v v] [s s])
                 (cond
                  [(zero? s) v]
                  [(positive? s) (loop (* v 2) (- s 1))]
                  [else (loop (quotient v 2) (+ s 1))])))]))
    (define (compute-not)
      (if (not (= (length expr*) 1))
          (s-error "Bad number of arguments for not"))
      (if (zero? (cf-eval-const (car expr*) env)) 1 0))
    (define (compute-or)
      (let loop ([arg* expr*])
        (cond
         [(null? arg*) 0]
         [(not (zero? (cf-eval-const (car arg*) env))) 1]
         [else (loop (cdr arg*))])))
    (define (compute-and)
      (let loop ([arg* expr*])
        (cond
         [(null? arg*) 1]
         [(zero? (cf-eval-const (car arg*) env)) 0]
         [else (loop (cdr arg*))])))
    (let ([v name]) ; ([v (cf-eval-param name env)])
      (case v
        [(size-of align-of) (check-id-arg* expr* 1)
         (let ([type (get-id (car expr*) env)])
           (ce-lookup env (list name type)
                      "Computing (~a ~a)" name type))]
        [(offset-of) (check-id-arg* expr* 2)
         (let ([type (get-id (car expr*) env)]
               [field (get-id (cadr expr*) env)])
           (ce-lookup env (list name type field)
                      "Computing (~a ~a ~a)" name type field))]
        ;; for sfc
        [(+) (compute-arith* 0 (lambda (x) x)     (lambda (a b) (+ a b)))]
        [(-) (compute-arith* 0 (lambda (x) (- x)) (lambda (a b) (- a b)))]
        [(*) (compute-arith* 1 (lambda (x) x)     (lambda (a b) (* a b)))]
        [(/) (compute-arith2 quotient)]
        [(=) (compute-equal)]
        [(shift) (compute-shift)]
        [(and) (compute-and)]
        [(or) (compute-or)]
        [(not) (compute-not)]
        [else (s-error "Unexpected constant operation ~a (~a)" name v)])))
  (define (cf-eval-param* param* env)
    (map (lambda (p) (cf-eval-param p env)) param*))
  (define (cf-eval-param param env)
    (ce-search-x env 'param param
                 (lambda (p) p)
                 (lambda () param)))
  (define (cf-eval-id id env)
    (ce-search-x env 'type id
                 (lambda (t)
                   (case t
                     [(param) (ce-lookup-x env 'param id
                                           "ICE in cf-eval-id/param")]
                     [(const) (ce-lookup-x env 'const id
                                           "ICE in cf-eval-id/const")]
                     [else id]))
                 (lambda () id)))
  (define (cf-eval-mname param env)
    (ce-search-x env 'param param
                 (lambda (p) (cf-eval-mname p env))
                 (lambda () param)))
  (variant-case ast
    [qa0-top (decl*)
      (let loop ([in* decl*] [out* '()] [env (ce-start-env machine real)])
        (cond
         [(null? in*) (values (make-qa0-top (reverse out*)) env)]
         [else (let-values* ([(out* env) (cf-decl (car in*) out* env)])
                            (loop (cdr in*) out* env))]))]))
