;; QCD to complex converter
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "ast.ss"
#fload "cenv.ss"
#fload "attr.ss"
;;
;; (provide qcd->complex)
;;
(define-syntax q2c-rename
  (syntax-rules ()
    [(_ env base type i ...) (q2c-rename* env base type (list i ...))]))

(define q2c-new-reg
  (let ([n 0])
    (lambda () (let ([r (gen-reg 'c n)])
                 (set! n (+ n 1))
                 r))))

(define (q2c-rename* env base type i*)
  (let ([key (list 'qcd->complex base type i*)])
    (ce-search env key
               (lambda (val)
                 (values val env))
               (lambda ()
                 (let* ([r (q2c-new-reg)]
                        [env (ce-bind env key r)])
                   (values r env))))))
(define qcd->complex
  (let ()
    (define (q2c-decl decl env)
      (variant-case decl
        [qa0-proc (attr* name arg-name* arg-type* arg-c-name* arg-c-type* code*)
          (q2c-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type*
                    code* env)]         
        [else (values decl env)]))
    (define (q2c-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type*
                      code* env)
      (let-values* ([(c* env) (q2c-code* code* env)])
        (values (make-qa0-proc attr* name arg-name* arg-type*
                               arg-c-name* arg-c-type* c*)
                env)))
    (define (q2c-code* code* env)
      (let loop ([r* '()] [code* code*] [env env])
        (cond
         [(null? code*) (values (reverse r*) env)]
         [else (let-values* ([(r* env) (q2c-code (car code*) r* env)])
                 (loop r* (cdr code*) env))])))
    (define (q2c-code c r* env)
      (variant-case c
        [qa0-operation (attr* name output* input*)
          (q2c-operation c attr* name output* input* r* env)]
        [qa0-load (attr* type output addr*)
          (q2c-load c attr* type output addr* r* env)]
        [qa0-store (attr* type addr* value)
          (q2c-store c attr* type addr* value r* env)]
        [qa0-loop (attr* var low high code*)
          (q2c-loop c attr* var low high code* r* env)]
        [qa0-if (var true-code* false-code*)
          (q2c-if c var true-code* false-code* r* env)]))
    (define (q2c-if c var true-code* false-code* r* env)
      (let-values* ([(t* env) (q2c-code* true-code* env)]
                    [(f* env) (q2c-code* false-code* env)])
        (values (cons (make-qa0-if var t* f*) r*)
                env)))
    (define (q2c-loop c attr* var low high code* r* env)
      (let-values* ([(c* env) (q2c-code* code* env)])
        (values (cons (make-qa0-loop attr* var low high c*) r*)
                env)))
    (define (q2c-store-su-n attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*colors*
                    'all 'gauge 'SU-n r* env))
    (define (q2c-store-su-n-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*colors*
                    'all 'gauge 'SU-n-double r* env))
    (define (q2c-store-su-n-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*colors*
                    'all 'gauge 'SU-n-float r* env))
    (define (q2c-store-fermion attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'all 'fermion 'Fermion r* env))
    (define (q2c-store-fermion-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'all 'fermion 'Fermion-double r* env))
    (define (q2c-store-fermion-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'all 'fermion 'Fermion-float r* env))
    (define (q2c-store-fermion-lo attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'low 'fermion 'Fermion r* env))
    (define (q2c-store-fermion-lo-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'low 'fermion 'Fermion-double r* env))
    (define (q2c-store-fermion-lo-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'low 'fermion 'Fermion-float r* env))
    (define (q2c-store-fermion-hi attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'high 'fermion 'Fermion r* env))
    (define (q2c-store-fermion-hi-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'high 'fermion 'Fermion-double r* env))
    (define (q2c-store-fermion-hi-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*fermion-dim*
                    'high 'fermion 'Fermion-float r* env))
    (define (q2c-store-projected-fermion attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*projected-fermion-dim*
                    'all 'projected-fermion 'Projected-Fermion r* env))
    (define (q2c-store-projected-fermion-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*projected-fermion-dim*
                    'all 'projected-fermion 'Projected-Fermion-double r* env))
    (define (q2c-store-projected-fermion-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* '*projected-fermion-dim*
                    'all 'projected-fermion 'Projected-Fermion-float r* env))
    (define (q2c-store-staggered-fermion attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* 1
                    'all 'staggered-fermion 'Staggered-Fermion r* env))
    (define (q2c-store-staggered-fermion-double attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* 1
                    'all 'staggered-fermion 'Staggered-Fermion-double r* env))
    (define (q2c-store-staggered-fermion-float attr* addr* value r* env)
      (q2c-store-xy attr* addr* value '*colors* 1
                    'all 'staggered-fermion 'Staggered-Fermion-float r* env))
    (define (q2c-store-xy attr* addr* value c-n f-n part t tt r* env)
      (let* ([c-n (ce-resolve-const env c-n "Color count")]
             [f-n (ce-resolve-const env f-n "Fermion size")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [c-t (ce-lookup-x env 'element-name tt "element type")]
             [c-s (ce-lookup-x env 'size-of c-t "element size")])
        (define (c-store c f r* env)
          (let-values* ([(x env) (q2c-rename env value t c f)]
                        [off (* c-s (+ f (* f-n c)))])
                       (values (cons (make-qa0-store attr*
                                       c-t
                                       (append addr*
                                               (list (make-c-expr-number off)))
                                       (make-reg x))
                                     r*)
                               env)))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* e) (c-store c f r* env)])
                            (f-loop (+ f 1) r* e))]))]))))
    (define (q2c-store-clover attr* addr* value r* env)
      (q2c-store-xyzt attr* addr* value 'clover 'Clover r* env))
    (define (q2c-store-clover-double attr* addr* value r* env)
      (q2c-store-xyzt attr* addr* value 'clover 'Clover-double r* env))
    (define (q2c-store-clover-float attr* addr* value r* env)
      (q2c-store-xyzt attr* addr* value 'clover 'Clover-float r* env))
    (define (q2c-store-xyzt attr* addr* value t tt r* env)
      (let* ([c-t (ce-lookup-x env 'element-name tt "element type")]
             [c-s (ce-lookup-x env 'size-of c-t "element size")]
             [cl-n (ce-resolve-const env '*clovers* "Clover size")]
             [cl-c (ce-resolve-const env '*colors* "Color count")]
             [cl-h (ce-resolve-const env '*projected-fermion-dim* "PF size")])
        (define (c-store p x-c y-h z-c t-h r* env)
          (let-values* ([(x env) (q2c-rename env value t p x-c y-h z-c t-h)]
                        [idx (if (eq? p 'low) 0 (* cl-n cl-n))]
                        [idx (+ idx t-h (* cl-h 
                                           (+ z-c
                                              (* cl-c
                                                 (+ y-h
                                                    (* cl-h x-c))))))]
                        [off (* c-s idx)])
            (values (cons (make-qa0-store attr* c-t
                                          (append addr*
                                                  (list (make-c-expr-number
                                                         off)))
                                          (make-reg x))
                          r*)
                    env)))
        (define (store-part p r* env)
          (let loop-x ([x 0] [r* r*] [env env])
            (cond
             [(= x cl-c) (values r* env)]
             [else
              (let loop-y ([y 0] [r* r*] [env env])
                (cond
                 [(= y cl-h) (loop-x (+ x 1) r* env)]
                 [else
                  (let loop-z ([z 0] [r* r*] [env env])
                    (cond
                     [(= z cl-c) (loop-y (+ y 1) r* env)]
                     [else
                      (let loop-t ([t 0] [r* r*] [env env])
                        (cond
                         [(= t cl-h) (loop-z (+ z 1) r* env)]
                         [else
                          (let-values ([(r* env) (c-store p x y z t r* env)])
                            (loop-t (+ t 1) r* env))]))]))]))])))
        (let-values* ([(r* env) (store-part 'low r* env)])
          (store-part 'high r* env))))
    (define q2c-store*
      (list
       (cons 'qcd-clover                   q2c-store-clover)
       (cons 'qcd-clover-double            q2c-store-clover-double)
       (cons 'qcd-clover-float             q2c-store-clover-float)
       (cons 'qcd-su-n                     q2c-store-su-n)
       (cons 'qcd-su-n-double              q2c-store-su-n-double)
       (cons 'qcd-su-n-float               q2c-store-su-n-float)
       (cons 'qcd-fermion                  q2c-store-fermion)
       (cons 'qcd-fermion-double           q2c-store-fermion-double)
       (cons 'qcd-fermion-float            q2c-store-fermion-float)
       (cons 'qcd-fermion-lo               q2c-store-fermion-lo)
       (cons 'qcd-fermion-lo-double        q2c-store-fermion-lo-double)
       (cons 'qcd-fermion-lo-float         q2c-store-fermion-lo-float)
       (cons 'qcd-fermion-hi               q2c-store-fermion-hi)
       (cons 'qcd-fermion-hi-double        q2c-store-fermion-hi-double)
       (cons 'qcd-fermion-hi-float         q2c-store-fermion-hi-float)
       (cons 'qcd-staggered-fermion        q2c-store-staggered-fermion)
       (cons 'qcd-staggered-fermion-double q2c-store-staggered-fermion-double)
       (cons 'qcd-staggered-fermion-float  q2c-store-staggered-fermion-float)
       (cons 'qcd-projected-fermion        q2c-store-projected-fermion)
       (cons 'qcd-projected-fermion-double q2c-store-projected-fermion-double)
       (cons 'qcd-projected-fermion-float  q2c-store-projected-fermion-float)))
    (define (q2c-store c attr* type addr* value r* env)
      (cond
       [(assq type q2c-store*)
        => (lambda (n&f) ((cdr n&f) attr* addr* value r* env))]
       [else (values (cons c r*) env)]))
    (define (q2c-load-su-n attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*colors*
                   'all 'gauge 'SU-n r* env))
    (define (q2c-load-su-n-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*colors*
                   'all 'gauge 'SU-n-double r* env))
    (define (q2c-load-su-n-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*colors*
                   'all 'gauge 'SU-n-float r* env))
    (define (q2c-load-fermion attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'all 'fermion 'Fermion r* env))
    (define (q2c-load-fermion-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'all 'fermion 'Fermion-double r* env))
    (define (q2c-load-fermion-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'all 'fermion 'Fermion-float r* env))
    (define (q2c-load-fermion-lo attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'low 'fermion 'Fermion r* env))
    (define (q2c-load-fermion-lo-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'low 'fermion 'Fermion-double r* env))
    (define (q2c-load-fermion-lo-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'low 'fermion 'Fermion-float r* env))
    (define (q2c-load-fermion-hi attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'high 'fermion 'Fermion r* env))
    (define (q2c-load-fermion-hi-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'high 'fermion 'Fermion-double r* env))
    (define (q2c-load-fermion-hi-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*fermion-dim*
                   'high 'fermion 'Fermion-float r* env))
    (define (q2c-load-staggered-fermion attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* 1
                   'all 'staggered-fermion 'Staggered-Fermion r* env))
    (define (q2c-load-staggered-fermion-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* 1
                   'all 'staggered-fermion 'Staggered-Fermion-double r* env))
    (define (q2c-load-staggered-fermion-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* 1
                   'all 'staggered-fermion 'Staggered-Fermion-float r* env))
    (define (q2c-load-projected-fermion attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*projected-fermion-dim*
                   'all 'projected-fermion 'Projected-Fermion r* env))
    (define (q2c-load-projected-fermion-double attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*projected-fermion-dim*
                   'all 'projected-fermion 'Projected-Fermion-double r* env))
    (define (q2c-load-projected-fermion-float attr* output addr* r* env)
      (q2c-load-xy attr* output addr* '*colors* '*projected-fermion-dim*
                   'all 'projected-fermion 'Projected-Fermion-float r* env))
    (define (q2c-load-xy attr* output addr* c-n f-n part t tt r* env)
      (let* ([c-n (ce-resolve-const env c-n "Color count")]
             [f-n (ce-resolve-const env f-n "Fermion size")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [c-t (ce-lookup-x env 'element-name tt "element type")]
             [c-s (ce-lookup-x env 'size-of c-t "element size")])
        (define (c-load c f r* env)
          (let-values* ([(x env) (q2c-rename env output t c f)]
                        [off (* c-s (+ f (* f-n c)))])
            (values (cons (make-qa0-load attr* c-t
                                         (make-reg x)
                                         (append addr*
                                                 (list (make-c-expr-number
                                                        off))))
                          r*)
                    env)))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* e) (c-load c f r* env)])
                                       (f-loop (+ f 1) r* e))]))]))))
    (define (q2c-load-clover attr* output addr* r* env)
      (q2c-load-xyzt attr* output addr* 'clover 'Clover r* env))
    (define (q2c-load-clover-double attr* output addr* r* env)
      (q2c-load-xyzt attr* output addr* 'clover 'Clover-double r* env))
    (define (q2c-load-clover-float attr* output addr* r* env)
      (q2c-load-xyzt attr* output addr* 'clover 'Clover-float r* env))
    (define (q2c-load-xyzt attr* output addr* t tt r* env)
      (let* ([c-t (ce-lookup-x env 'element-name tt "element type")]
             [c-s (ce-lookup-x env 'size-of c-t "element size")]
             [cl-n (ce-resolve-const env '*clovers* "Clover size")]
             [cl-c (ce-resolve-const env '*colors* "Color count")]
             [cl-h (ce-resolve-const env '*projected-fermion-dim* "PF size")])
        (define (c-load p x-c y-h z-c t-h r* env)
          (let-values* ([(x env) (q2c-rename env output t p x-c y-h z-c t-h)]
                        [idx (if (eq? p 'low) 0 (* cl-n cl-n))]
                        [idx (+ idx t-h (* cl-h 
                                           (+ z-c
                                              (* cl-c
                                                 (+ y-h
                                                    (* cl-h x-c))))))]
                        [off (* c-s idx)])
            (values (cons (make-qa0-load attr* c-t (make-reg x)
                                         (append addr*
                                                 (list (make-c-expr-number
                                                        off))))
                          r*)
                    env)))
        (define (load-part p r* env)
          (let loop-x ([x 0] [r* r*] [env env])
            (cond
             [(= x cl-c) (values r* env)]
             [else
              (let loop-y ([y 0] [r* r*] [env env])
                (cond
                 [(= y cl-h) (loop-x (+ x 1) r* env)]
                 [else
                  (let loop-z ([z 0] [r* r*] [env env])
                    (cond
                     [(= z cl-c) (loop-y (+ y 1) r* env)]
                     [else
                      (let loop-t ([t 0] [r* r*] [env env])
                        (cond
                         [(= t cl-h) (loop-z (+ z 1) r* env)]
                         [else
                          (let-values ([(r* env) (c-load p x y z t r* env)])
                            (loop-t (+ t 1) r* env))]))]))]))])))
        (let-values* ([(r* env) (load-part 'low r* env)])
          (load-part 'high r* env))))
    (define q2c-load*
      (list
       (cons 'qcd-clover                   q2c-load-clover)
       (cons 'qcd-clover-double            q2c-load-clover-double)
       (cons 'qcd-clover-float             q2c-load-clover-float)
       (cons 'qcd-su-n                     q2c-load-su-n)
       (cons 'qcd-su-n-double              q2c-load-su-n-double)
       (cons 'qcd-su-n-float               q2c-load-su-n-float)
       (cons 'qcd-projected-fermion        q2c-load-projected-fermion)
       (cons 'qcd-projected-fermion-double q2c-load-projected-fermion-double)
       (cons 'qcd-projected-fermion-float  q2c-load-projected-fermion-float)
       (cons 'qcd-staggered-fermion        q2c-load-staggered-fermion)
       (cons 'qcd-staggered-fermion-double q2c-load-staggered-fermion-double)
       (cons 'qcd-staggered-fermion-float  q2c-load-staggered-fermion-float)
       (cons 'qcd-fermion                  q2c-load-fermion)
       (cons 'qcd-fermion-double           q2c-load-fermion-double)
       (cons 'qcd-fermion-float            q2c-load-fermion-float)
       (cons 'qcd-fermion-lo               q2c-load-fermion-lo)
       (cons 'qcd-fermion-lo-double        q2c-load-fermion-lo-double)
       (cons 'qcd-fermion-lo-float         q2c-load-fermion-lo-float)
       (cons 'qcd-fermion-hi               q2c-load-fermion-hi)
       (cons 'qcd-fermion-hi-double        q2c-load-fermion-hi-double)
       (cons 'qcd-fermion-hi-float         q2c-load-fermion-hi-float)))
    (define (q2c-load c attr* type output addr* r* env)
      (cond
       [(assq type q2c-load*)
        => (lambda (n&f) ((cdr n&f) attr* output addr* r* env))]
       [else (values (cons c r*) env)]))
    (define (q2c-su-n-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*colors*
                  'gauge 'SU-n r* env))
    (define (q2c-su-n-double-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*colors*
                  'gauge 'SU-n-double r* env))
    (define (q2c-su-n-float-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*colors*
                  'gauge 'SU-n-float r* env))
    (define (q2c-fermion-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*fermion-dim*
                  'fermion 'Fermion r* env))
    (define (q2c-fermion-double-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*fermion-dim*
                  'fermion 'Fermion-double r* env))
    (define (q2c-fermion-float-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*fermion-dim*
                  'fermion 'Fermion-float r* env))
    (define (q2c-projected-fermion-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*projected-fermion-dim*
                  'projected-fermion 'Projected-Fermion r* env))
    (define (q2c-projected-fermion-double-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*projected-fermion-dim*
                  'projected-fermion 'Projected-Fermion-double r* env))
    (define (q2c-projected-fermion-float-offset attr* output* input* r* env)
      (q2c-offset attr* output* input* '*colors* '*projected-fermion-dim*
                  'projected-fermion 'Projected-Fermion-float r* env))
    (define (q2c-offset attr* output* input* c-n f-n t tt r* env)
      (let* ([c-n (ce-resolve-const env c-n "Color count")]
             [f-n (ce-resolve-const env f-n "Fermion size")]
             [e-t (ce-lookup-x env 'element-name tt "element name")]
             [d-n (ce-lookup-x env 'size-of e-t "complex size")]
             [c   (car input*)] [f (cadr input*)] [r (car output*)]
             [r0  (make-reg (q2c-new-reg))] [m0 (* d-n f-n)]
             [r1  (make-reg (q2c-new-reg))] [m1 d-n])
        (values (list* (make-qa0-operation '() 'int-add output* (list r0 r1))
                       (make-qa0-operation '() 'int-mul (list r1)
                                           (list f (make-c-expr-number m1)))
                       (make-qa0-operation '() 'int-mul (list r0)
                                           (list c (make-c-expr-number m0)))
                       r*)
                env)))
    (define (q2c-staggered-fermion-offset attr* output* input* r* env)
      (q2c-offset-1 attr* output* input* 'COMPLEX r* env))
    (define (q2c-staggered-fermion-double-offset attr* output* input* r* env)
      (q2c-offset-1 attr* output* input* 'complex-double r* env))
    (define (q2c-staggered-fermion-float-offset attr* output* input* r* env)
      (q2c-offset-1 attr* output* input* 'complex-float r* env))
    (define (q2c-offset-1 attr* output* input* tt r* env)
      (let* ([d-n (ce-lookup-x env 'size-of tt "complex size")]
             [c   (car input*)] [r (car output*)])
        (values (list* (make-qa0-operation '() 'int-mul (list r)
                                           (list c (make-c-expr-number d-n)))
                       r*)
                env)))
    (define (q2c-clover-lo-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'low 'COMPLEX r* env))
    (define (q2c-clover-lo-double-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'low 'complex-double r* env))
    (define (q2c-clover-lo-float-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'low 'complex-float r* env))
    (define (q2c-clover-hi-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'high 'COMPLEX r* env))
    (define (q2c-clover-hi-double-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'high 'complex-double r* env))
    (define (q2c-clover-hi-float-offset attr* output* input* r* env)
      (q2c-offset-4 attr* output* input* 'high 'complex-float r* env))
    (define (q2c-offset-4 attr* output* input* p tt r* env)
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [h-n (ce-resolve-const env '*projected-fermion-dim* "PF size")]
             [cl-n (ce-resolve-const env '*clovers* "Clover size")]
             [c-s (ce-lookup-x env 'size-of tt "complex size")]
             [t (cadddr input*)] [ft c-s]
             [z (caddr input*)]  [fz (* ft h-n)]
             [y (cadr input*)]   [fy (* fz c-n)]
             [x (car input*)]    [fx (* fy h-n)]
             [rt (q2c-new-reg)] [st (make-reg rt)]
             [rz (q2c-new-reg)] [sz (make-reg rz)]
             [ry (q2c-new-reg)] [sy (make-reg ry)]
             [rx (q2c-new-reg)] [sx (make-reg rx)]
             [r1 (q2c-new-reg)] [s1 (make-reg r1)]
             [r2 (q2c-new-reg)] [s2 (make-reg r2)]
             [r3 (q2c-new-reg)] [s3 (make-reg r3)]             [fp (* fx c-n)])
        (values
         (if (eq? p 'low)
             (list* (make-qa0-operation '() 'int-add output* (list s1 s2))
                    (make-qa0-operation '() 'int-add (list s1) (list sx sy))
                    (make-qa0-operation '() 'int-add (list s2) (list sz st))
                    (make-qa0-operation '() 'int-mul (list sx)
                                        (list x (make-c-expr-number fx)))
                    (make-qa0-operation '() 'int-mul (list sy)
                                        (list y (make-c-expr-number fy)))
                    (make-qa0-operation '() 'int-mul (list sz)
                                        (list z (make-c-expr-number fz)))
                    (make-qa0-operation '() 'int-mul (list st)
                                        (list t (make-c-expr-number ft)))
                    r*)
             (list* (make-qa0-operation '() 'int-add output*
                                        (list s3 (make-c-expr-number fp)))
                    (make-qa0-operation '() 'int-add (list s3) (list s1 s2))
                    (make-qa0-operation '() 'int-add (list s1) (list sx sy))
                    (make-qa0-operation '() 'int-add (list s2) (list sz st))
                    (make-qa0-operation '() 'int-mul (list sx)
                                        (list x (make-c-expr-number fx)))
                    (make-qa0-operation '() 'int-mul (list sy)
                                        (list y (make-c-expr-number fy)))
                    (make-qa0-operation '() 'int-mul (list sz)
                                        (list z (make-c-expr-number fz)))
                    (make-qa0-operation '() 'int-mul (list st)
                                        (list t (make-c-expr-number ft)))
                    r*))
                env)))
    (define (q2c-addu attr* output* input* r* env)
      (q2c-addx 'complex-add attr* output* input* '*colors*
                'gauge r* env))
    (define (q2c-addf attr* output* input* r* env)
      (q2c-addx 'complex-add attr* output* input* '*fermion-dim*
                'fermion r* env))
    (define (q2c-addh attr* output* input* r* env)
      (q2c-addx 'complex-add attr* output* input* '*projected-fermion-dim*
                'projected-fermion r* env))
    (define (q2c-adds attr* output* input* r* env)
      (q2c-addx 'complex-add attr* output* input* 1
                'staggered-fermion r* env))
    (define (q2c-subu attr* output* input* r* env)
      (q2c-addx 'complex-sub attr* output* input* '*colors*
                'gauge r* env))
    (define (q2c-subf attr* output* input* r* env)
      (q2c-addx 'complex-sub attr* output* input* '*fermion-dim*
                'fermion r* env))
    (define (q2c-subh attr* output* input* r* env)
      (q2c-addx 'complex-sub attr* output* input* '*projected-fermion-dim*
                'projected-fermion r* env))
    (define (q2c-subs attr* output* input* r* env)
      (q2c-addx 'complex-sub attr* output* input* 1
                'staggered-fermion r* env))
    (define (q2c-mulf attr* output* input* r* env)
      (q2c-mulx attr* output* input* '*fermion-dim*
                q2c-get-fermion r* env))
    (define (q2c-mulh attr* output* input* r* env)
      (q2c-mulx attr* output* input* '*projected-fermion-dim*
                q2c-get-projected-fermion r* env))
    (define (q2c-muls attr* output* input* r* env)
      (q2c-mulx attr* output* input* 1
                q2c-get-staggered-fermion r* env))
    (define (q2c-mulf-conj attr* output* input* r* env)
      (q2c-mulx-conj attr* output* input* '*fermion-dim*
                     q2c-get-fermion r* env))
    (define (q2c-mulh-conj attr* output* input* r* env)
      (q2c-mulx-conj attr* output* input* '*projected-fermion-dim*
                     q2c-get-projected-fermion r* env))
    (define (q2c-muls-conj attr* output* input* r* env)
      (q2c-mulx-conj attr* output* input* 1
                     q2c-get-staggered-fermion r* env))
    (define (q2c-u-mul attr* output* input* r* env)
      (q2c-mulu attr* output* input*
                q2c-get-gauge q2c-get-gauge
                'complex-mul 'complex-madd
                r* env))
    (define (q2c-u-conj-mul attr* output* input* r* env)
      (q2c-mulu attr* output* input*
                q2c-get-conj-gauge q2c-get-gauge
                'complex-cmul 'complex-cmadd
                r* env))
    (define (q2c-u-mul-conj attr* output* input* r* env)
      (q2c-mulu attr* output* input*
                q2c-get-gauge q2c-get-conj-gauge
                'complex-c2mul 'complex-c2madd
                r* env))
    (define (q2c-u-conj-mul-conj attr* output* input* r* env)
      (q2c-mulu attr* output* input*
                q2c-get-conj-gauge q2c-get-conj-gauge
                'complex-ccmul 'complex-ccmadd
                r* env))
    (define (q2c-mul-clover attr* output* input* r* env)
      (q2c-mulx-clover attr* output* input* r* env
                       'complex-mul 'complex-madd q2c-get-clover))
    (define (q2c-conj-mul-clover attr* output* input* r* env)
      (q2c-mulx-clover attr* output* input* r* env
                       'complex-cmul 'complex-cmadd q2c-get-transpose-clover))
    (define (q2c-mulx-clover attr* output* input* r* env op-a op-b get-clover)
      (define (do-part p r* env)
        (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
               [p-n (ce-resolve-const env '*projected-fermion-dim* "PF dim")]
               [f-n (ce-resolve-const env '*fermion-dim* "Fermion dim")]
               [b-lo (if (eq? p 'low) 0 p-n)]
               [b-hi (if (eq? p 'low) p-n f-n)]
               [r-C (car input*)]
               [r-b (cadr input*)]
               [r-r (car output*)])
          (define (get-result a id b j env)
            (cond
             [(and (= b (- c-n 1)) (= j (- p-n 1)))
              (q2c-get-fermion r-r a id env)]
             [else (q2c-get-fermion (q2c-new-reg) a id env)]))
          (define (get-op a id b j)
            (cond
             [(and (= b 0) (= j 0)) #t]
             [(and (= c-n 1) (= p-n 1)) #t]
             [else #f]))
          (define (do-op op r-v r-x C-v b-v r*)
            (cond
             [op (cons (make-qa0-operation attr* op-a
                                           (list (make-reg r-v))
                                           (list (make-reg C-v)
                                                 (make-reg b-v)))
                       r*)]
             [else (cons (make-qa0-operation attr* op-b
                                             (list (make-reg r-v))
                                             (list (make-reg r-x)
                                                   (make-reg C-v)
                                                   (make-reg b-v)))
                       r*)]))
          (define (do-one p a i id r* env)
            (let loop-b ([b 0] [r* r*] [env env] [r-x #f])
              (cond
               [(= b c-n) (values r* env)]
               [else
                (let loop-j ([j 0] [jd b-lo] [r* r*] [env env] [r-x r-x])
                  (cond
                   [(= j p-n) (loop-b (+ b 1) r* env r-x)]
                   [else
                    (let-values* ([(b-v env) (q2c-get-fermion r-b b jd env)]
                                  [(C-v env) (get-clover r-C p a i b j env)]
                                  [(r-v env) (get-result a id b j env)]
                                  [op (get-op a id b j)]
                                  [r* (do-op op r-v r-x C-v b-v r*)])
                      (loop-j (+ j 1) (+ jd 1) r* env r-v))]))])))
          (let loop-a ([a 0] [r* r*] [env env])
            (cond
             [(= a c-n) (values r* env)]
             [else
              (let loop-i ([i 0] [id b-lo] [r* r*] [env env])
                (cond
                 [(= i p-n) (loop-a (+ a 1) r* env)]
                 [else
                  (let-values ([(r* env) (do-one p a i id r* env)])
                    (loop-i (+ i 1) (+ id 1) r* env))]))]))))
      (let-values ([(r* env) (do-part 'low r* env)])
        (do-part 'high r* env)))
    (define (q2c-u-retr-conj-mul attr* output* input* r* env)
      (q2c-check-list output* 1 "qcd-su-n-real-trace-conj-mul outputs")
      (q2c-check-list input* 2 "qcd-su-n-real-trace-conj-mul inputs")
      (let ([c-n (ce-resolve-const env '*colors* "Color count")]
            [r-r (car output*)]
            [r-a (car input*)]
            [r-b (cadr input*)])
        (define (step x y r* env q)
          (let-values* ([(v-a env) (q2c-get-conj-gauge r-a x y env)]
                        [(v-b env) (q2c-get-gauge r-b x y env)]
                        [(p) (make-reg (q2c-new-reg))])
            (values (cons (if (and (zero? x) (zero? y))
                              (make-qa0-operation
                               attr*
                               'complex-real-cmul-conj-init
                               (list p)
                               (list (make-reg v-a) (make-reg v-b)))
                              (make-qa0-operation
                               attr*
                               'complex-real-cmul-conj-add
                               (list p)
                               (list q (make-reg v-a) (make-reg v-b))))
                          r*)
                    env
                    p)))
        (let loop-x ([x 0] [r* r*] [env env] [q #f])
          (cond
           [(= x c-n) (values (cons (make-qa0-operation
                                     attr*
                                     'complex-real-cmul-conj-fini
                                     (list r-r)
                                     (list q))
                                    r*)
                              env)]
           [else
            (let loop-y ([y 0] [r* r*] [env env] [q q])
              (cond
               [(= y c-n) (loop-x (+ x 1) r* env q)]
               [else (let-values ([(r* env q) (step x y r* env q)])
                       (loop-y (+ y 1) r* env q))]))]))))
    (define (q2c-cmaddf attr* output* input* r* env)
      (q2c-cmaddx attr* output* input* '*fermion-dim*
                  'all 'fermion r* env))
    (define (q2c-cmaddf-lo attr* output* input* r* env)
      (q2c-cmaddx attr* output* input* '*fermion-dim*
                  'low 'fermion r* env))
    (define (q2c-cmaddf-hi attr* output* input* r* env)
      (q2c-cmaddx attr* output* input* '*fermion-dim*
                  'high 'fermion r* env))
    (define (q2c-cmaddh attr* output* input* r* env)
      (q2c-cmaddx attr* output* input* '*projected-fermion-dim*
                  'all 'projected-fermion r* env))
    (define (q2c-cmadds attr* output* input* r* env)
      (q2c-cmaddx attr* output* input* 1
                  'all 'staggered-fermion r* env))
    (define (q2c-maddf attr* output* input* r* env)
      (q2c-maddx attr* output* input* '*fermion-dim*
                 'all 'fermion r* env))
    (define (q2c-maddf-lo attr* output* input* r* env)
      (q2c-maddx attr* output* input* '*fermion-dim*
                 'low 'fermion r* env))
    (define (q2c-maddf-hi attr* output* input* r* env)
      (q2c-maddx attr* output* input* '*fermion-dim*
                 'high 'fermion r* env))
    (define (q2c-maddh attr* output* input* r* env)
      (q2c-maddx attr* output* input* '*projected-fermion-dim*
                 'all 'projected-fermion r* env))
    (define (q2c-madds attr* output* input* r* env)
      (q2c-maddx attr* output* input* 1
                 'all 'staggered-fermion r* env))
    (define (q2c-msubf attr* output* input* r* env)
      (q2c-msubx attr* output* input* '*fermion-dim*
                 'all 'fermion r* env))
    (define (q2c-msubf-lo attr* output* input* r* env)
      (q2c-msubx attr* output* input* '*fermion-dim*
                 'low 'fermion r* env))
    (define (q2c-msubf-hi attr* output* input* r* env)
      (q2c-msubx attr* output* input* '*fermion-dim*
                 'high 'fermion r* env))
    (define (q2c-msubh attr* output* input* r* env)
      (q2c-msubx attr* output* input* '*projected-fermion-dim*
                 'all 'projected-fermion r* env))
    (define (q2c-msubs attr* output* input* r* env)
      (q2c-msubx attr* output* input* 1
                 'all 'staggered-fermion r* env))
    (define (q2c-cmsubf attr* output* input* r* env)
      (q2c-cmsubx attr* output* input* '*fermion-dim*
                  'all 'fermion r* env))
    (define (q2c-cmsubf-lo attr* output* input* r* env)
      (q2c-cmsubx attr* output* input* '*fermion-dim*
                  'low 'fermion r* env))
    (define (q2c-cmsubf-hi attr* output* input* r* env)
      (q2c-cmsubx attr* output* input* '*fermion-dim*
                  'high 'fermion r* env))
    (define (q2c-zerou attr* output* input* r* env)
      (q2c-zerox attr* output* input* '*colors*
                 'all 'gauge r* env))
    (define (q2c-zerof attr* output* input* r* env)
      (q2c-zerox attr* output* input* '*fermion-dim*
                 'all 'fermion r* env))
    (define (q2c-zeroh attr* output* input* r* env)
      (q2c-zerox attr* output* input* '*projected-fermion-dim*
                 'all 'projected-fermion r* env))
    (define (q2c-zeros attr* output* input* r* env)
      (q2c-zerox attr* output* input* 1
                 'all 'staggered-fermion r* env))
    (define (q2c-scaleu attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* '*colors*
                  'all 'gauge r* env))
    (define (q2c-scalef attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* '*fermion-dim*
                  'all 'fermion r* env))
    (define (q2c-scalef-lo attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* '*fermion-dim*
                  'low 'fermion r* env))
    (define (q2c-scalef-hi attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* '*fermion-dim*
                  'high 'fermion r* env))
    (define (q2c-scaleh attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* '*projected-fermion-dim*
                  'all 'projected-fermion r* env))
    (define (q2c-scales attr* output* input* r* env)
      (q2c-scalex 'complex-rmul attr* output* input* 1
                  'all 'staggered-fermion r* env))
    (define (q2c-complex-scaleu attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* '*colors*
                  'all 'gauge r* env))
    (define (q2c-complex-scalef attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* '*fermion-dim*
                  'all 'fermion r* env))
    (define (q2c-complex-scalef-lo attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* '*fermion-dim*
                  'low 'fermion r* env))
    (define (q2c-complex-scalef-hi attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* '*fermion-dim*
                  'high 'fermion r* env))
    (define (q2c-complex-scaleh attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* '*projected-fermion-dim*
                  'all 'projected-fermion r* env))
    (define (q2c-complex-scales attr* output* input* r* env)
      (q2c-scalex 'complex-mul attr* output* input* 1
                  'all 'staggered-fermion r* env))
    (define (q2c-complex-conj-scaleu attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* '*colors*
                  'all 'gauge r* env))
    (define (q2c-complex-conj-scalef attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* '*fermion-dim*
                  'all 'fermion r* env))
    (define (q2c-complex-conj-scalef-lo attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* '*fermion-dim*
                  'low 'fermion r* env))
    (define (q2c-complex-conj-scalef-hi attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* '*fermion-dim*
                  'high 'fermion r* env))
    (define (q2c-complex-conj-scaleh attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* '*projected-fermion-dim*
                  'all 'projected-fermion r* env))
    (define (q2c-complex-conj-scales attr* output* input* r* env)
      (q2c-scalex 'complex-cmul attr* output* input* 1
                  'all 'staggered-fermion r* env))
    (define (q2c-fnorm-init attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD fermion norm init outputs")
      (q2c-check-list input* 0 "QCD fermion norm init inputs")
      (values (cons (make-qa0-operation attr*
                                        'complex-norm-init
                                        output*
                                        input*)
                    r*)
              env))
    (define (q2c-snorm-init attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD staggered fermion norm init outputs")
      (q2c-check-list input* 0 "QCD staggered fermion norm init inputs")
      (values (cons (make-qa0-operation attr*
                                        'complex-norm-init
                                        output*
                                        input*)
                    r*)
              env))
    (define (q2c-fnorm-lo-add attr* output* input* r* env)
      (q2c-xnorm-add-do attr* output* input* r* env
                        'low '*fermion-dim* 'fermion))
    (define (q2c-fnorm-hi-add attr* output* input* r* env)
      (q2c-xnorm-add-do attr* output* input* r* env
                        'high '*fermion-dim* 'fermion))
    (define (q2c-fnorm-add attr* output* input* r* env)
      (q2c-xnorm-add-do attr* output* input* r* env
                        'all '*fermion-dim* 'fermion))
    (define (q2c-snorm-add attr* output* input* r* env)
      (q2c-xnorm-add-do attr* output* input* r* env
                        'all 1 'staggered-fermion))
    (define (q2c-xnorm-add-do attr* output* input* r* env part f-dim f-kind)
      (define (complex-norm c f r* env)
        (let-values* ([(a env) (q2c-rename env (cadr input*) f-kind c f)])
                     (values (cons (make-qa0-operation attr*
                                     'complex-norm-add
                                     output*
                                     (list (car input*) (make-reg a)))
                                   r*)
                             env)))
      (q2c-check-list output* 1 "QCD fermion norm add outputs")
      (q2c-check-list input* 2 "QCD fermion norm add inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-dim "Fermion dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (complex-norm c f r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-fnorm-fini attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD fermion norm fini outputs")
      (q2c-check-list input* 1 "QCD fermion norm fini inputs")
      (values (cons (make-qa0-operation attr* 'complex-norm-fini output* input*)
                    r*)
              env))
    (define (q2c-snorm-fini attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD staggered fermion norm fini outputs")
      (q2c-check-list input* 1 "QCD staggered fermion norm fini inputs")
      (values (cons (make-qa0-operation attr* 'complex-norm-fini output* input*)
                    r*)
              env))
    (define (q2c-fdot-init attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD fermion dot init outputs")
      (q2c-check-list input* 0 "QCD fermion dot init inputs")
      (values (cons (make-qa0-operation attr* 'complex-dot-init output* input*)
                    r*)
              env))
    (define (q2c-sdot-init attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD staggered fermion dot init outputs")
      (q2c-check-list input* 0 "QCD staggered fermion dot init inputs")
      (values (cons (make-qa0-operation attr* 'complex-dot-init output* input*)
                    r*)
              env))
    (define (q2c-fdot-add attr* output* input* r* env)
      (q2c-xdot-add-do attr* output* input* r* env '*fermion-dim* 'fermion))
    (define (q2c-sdot-add attr* output* input* r* env)
      (q2c-xdot-add-do attr* output* input* r* env 1 'staggered-fermion))
    (define (q2c-xdot-add-do attr* output* input* r* env f-dim f-kind)
      (define (complex-dot c f r* env)
        (let-values* ([(a env) (q2c-rename env (cadr input*) f-kind c f)]
                      [(b env) (q2c-rename env (caddr input*) f-kind c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-cmadd
                          output* (list (car input*) (make-reg a) (make-reg b)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD fermion dot add outputs")
      (q2c-check-list input* 3 "QCD fermion dot add inputs")
      (let ([c-n (ce-resolve-const env '*colors* "Color count")]
            [f-n (ce-resolve-const env f-dim "Fermion dimension")])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f 0] [r* r*] [env env])
                   (cond
                    [(= f f-n) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (complex-dot c f r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-fdot-fini attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD fermion dot fini outputs")
      (q2c-check-list input* 1 "QCD fermion dot fini inputs")
      (values (cons (make-qa0-operation attr* 'complex-dot-fini output* input*)
                    r*)
              env))
    (define (q2c-sdot-fini attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD staggered fermion dot fini outputs")
      (q2c-check-list input* 1 "QCD staggered fermion dot fini inputs")
      (values (cons (make-qa0-operation attr* 'complex-dot-fini output* input*)
                    r*)
              env))
    (define (q2c-addx c-op attr* output* input* f-n t r* env)
      (define (complex-add c f r* env)
        (let-values* ([(a env) (q2c-rename env (car input*) t c f)]
                      [(b env) (q2c-rename env (cadr input*) t c f)]
                      [(d env) (q2c-rename env (car output*) t c f)])
                     (values (cons (make-qa0-operation attr*
                                     c-op
                                     (list (make-reg d))
                                     (list (make-reg a) (make-reg b)))
                                   r*)
                             env)))
      (q2c-check-list output* 1 "QCD add outputs")
      (q2c-check-list input* 2 "QCD add inputs")
      (let ([c-n (ce-resolve-const env '*colors* "Color count")]
            [f-n (ce-resolve-const env f-n "Field dimension")])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f 0] [r* r*] [env env])
                   (cond
                    [(= f f-n) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (complex-add c f r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-zerox attr* output* input* f-n part t r* env)
      (define (complex-zero c f r* env)
        (let-values* ([(r env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-zero (list (make-reg r)) '())
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD zero outputs")
      (q2c-check-list input* 0 "QCD zero inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (complex-zero c f r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-scalex op attr* output* input* f-n part t r* env)
      (define (complex-scale c f r* env)
        (let-values* ([a (car input*)]
                      [(b env) (q2c-rename env (cadr input*) t c f)]
                      [(r env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          op
                          (list (make-reg r))
                          (list a (make-reg b)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD scale outputs")
      (q2c-check-list input* 2 "QCD scale inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (complex-scale c f r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-madd-lohi attr* output* input* r* env)
      (let ([t 'fermion])
        (define (complex-madd c f s alpha a r* env)
          (let-values* ([(s env) (q2c-rename env s t c f)]
                        [(a env) (q2c-rename env a t c f)]
                        [(x env) (q2c-rename env (car output*) t c f)])
            (values (cons (make-qa0-operation attr*
                            'complex-rmadd
                            (list (make-reg x))
                            (list (make-reg s) alpha (make-reg a)))
                          r*)
                    env)))
        (q2c-check-list output* 1 "QCD madd-lohi outputs")
        (q2c-check-list input* 5 "QCD madd-lohi inputs")
        (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
               [f-n (ce-resolve-const env '*fermion-dim* "Field dimension")]
               [f-m (/ f-n 2)]
               [s  (car input*)]
               [alpha (cadr input*)] [a (caddr input*)]
               [beta (cadddr input*)] [b (car (cddddr input*))])
          (let c-loop ([c 0] [r* r*] [env env])
            (cond
             [(= c c-n) (values r* env)]
             [else
              (let f-loop ([f 0] [r* r*] [env env])
                (cond
                 [(= f f-n) (c-loop (+ c 1) r* env)]
                 [else
                  (let-values*
                      ([(r* env) (if (< f f-m)
                                     (complex-madd c f s alpha a r* env)
                                     (complex-madd c f s beta b r* env))])
                    (f-loop (+ f 1) r* env))]))])))))
    (define (q2c-msub-lohi attr* output* input* r* env)
      (let ([t 'fermion])
        (define (complex-msub c f s alpha a r* env)
          (let-values* ([(s env) (q2c-rename env s t c f)]
                        [(a env) (q2c-rename env a t c f)]
                        [(x env) (q2c-rename env (car output*) t c f)])
            (values (cons (make-qa0-operation attr*
                            'complex-rmsub
                            (list (make-reg x))
                            (list (make-reg s) alpha (make-reg a)))
                          r*)
                    env)))
        (q2c-check-list output* 1 "QCD msub-lohi outputs")
        (q2c-check-list input* 5 "QCD msub-lohi inputs")
        (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
               [f-n (ce-resolve-const env '*fermion-dim* "Field dimension")]
               [f-m (/ f-n 2)]
               [s  (car input*)]
               [alpha (cadr input*)] [a (caddr input*)]
               [beta (cadddr input*)] [b (car (cddddr input*))])
          (let c-loop ([c 0] [r* r*] [env env])
            (cond
             [(= c c-n) (values r* env)]
             [else
              (let f-loop ([f 0] [r* r*] [env env])
                (cond
                 [(= f f-n) (c-loop (+ c 1) r* env)]
                 [else
                  (let-values*
                      ([(r* env) (if (< f f-m)
                                     (complex-msub c f s alpha a r* env)
                                     (complex-msub c f s beta b r* env))])
                    (f-loop (+ f 1) r* env))]))])))))
    (define (q2c-cmaddx attr* output* input* f-n part t r* env)
      (define (complex-cmadd c f s alpha a r* env)
        (let-values* ([(s env) (q2c-rename env s t c f)]
                      [(a env) (q2c-rename env a t c f)]
                      [(x env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-madd
                          (list (make-reg x))
                          (list (make-reg a) alpha (make-reg s)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD cmadd outputs")
      (q2c-check-list input* 3 "QCD cmadd inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [a  (car input*)]
             [alpha (cadr input*)]
             [s (caddr input*)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values*
                              ([(r* env) (complex-cmadd c f s alpha a r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-maddx attr* output* input* f-n part t r* env)
      (define (complex-madd c f s alpha a r* env)
        (let-values* ([(s env) (q2c-rename env s t c f)]
                      [(a env) (q2c-rename env a t c f)]
                      [(x env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-rmadd
                          (list (make-reg x))
                          (list (make-reg a) alpha (make-reg s)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD madd outputs")
      (q2c-check-list input* 3 "QCD madd inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [a  (car input*)]
             [alpha (cadr input*)]
             [s (caddr input*)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values*
                              ([(r* env) (complex-madd c f s alpha a r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-msubx attr* output* input* f-n part t r* env)
      (define (complex-msub c f s alpha a r* env)
        (let-values* ([(s env) (q2c-rename env s t c f)]
                      [(a env) (q2c-rename env a t c f)]
                      [(x env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-rmsub
                          (list (make-reg x))
                          (list (make-reg a) alpha (make-reg s)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD msub outputs")
      (q2c-check-list input* 3 "QCD msub inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [a  (car input*)]
             [alpha (cadr input*)]
             [s (caddr input*)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values*
                              ([(r* env) (complex-msub c f s alpha a r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-cmsubx attr* output* input* f-n part t r* env)
      (define (complex-cmsub c f s alpha a r* env)
        (let-values* ([(s env) (q2c-rename env s t c f)]
                      [(a env) (q2c-rename env a t c f)]
                      [(x env) (q2c-rename env (car output*) t c f)])
          (values (cons (make-qa0-operation attr*
                          'complex-msub
                          (list (make-reg x))
                          (list (make-reg a) alpha (make-reg s)))
                        r*)
                  env)))
      (q2c-check-list output* 1 "QCD cmsub outputs")
      (q2c-check-list input* 3 "QCD cmsub inputs")
      (let* ([c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env f-n "Field dimension")]
             [f-lo (if (eq? part 'high) (/ f-n 2) 0)]
             [f-hi (if (eq? part 'low) (/ f-n 2) f-n)]
             [a  (car input*)]
             [alpha (cadr input*)]
             [s (caddr input*)])
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f f-lo] [r* r*] [env env])
                   (cond
                    [(= f f-hi) (c-loop (+ c 1) r* env)]
                    [else (let-values*
                              ([(r* env) (complex-cmsub c f s alpha a r* env)])
                            (f-loop (+ f 1) r* env))]))]))))
    (define (q2c-mul-g attr* output* input* f-n
                       r-get op-0 op-k u-get f-get r* env)
      (q2c-check-list output* 1 "QCD mul outputs")
      (q2c-check-list input* 2 "QCD mul inputs")
      (let ([c-n (ce-resolve-const env '*colors* "Color count")]
            [f-n (ce-resolve-const env f-n "Field dimension")]
            [r-U (car input*)]
            [r-b (cadr input*)]
            [r-r (car output*)])
        (define (s-mul-z r* env)
          (let-values* ([(U-v env) (u-get r-U 0 0 env)])
            (let loop ([f 0] [r* r*] [env env])
              (cond
               [(= f f-n) (values r* env)]
               [else (let-values* ([(b env) (f-get r-b 0 f env)]
                                   [(z env) (r-get r-r 0 f env)])
                       (loop (+ f 1)
                             (cons (make-qa0-operation attr*
                                     op-0
                                     (list (make-reg z))
                                     (list (make-reg U-v) (make-reg b)))
                                   r*)
                             env))]))))
        (define (s-mul-1 r* env)
          (let ([q (q2c-new-reg)])
            (let x-loop ([x 0] [r* r*] [env env])
              (cond
               [(= x c-n) (values r* env q)]
               [else
                (let y-loop ([y 0] [r* r*] [env env])
                  (cond
                   [(= y f-n) (x-loop (+ x 1) r* env)]
                   [else
                    (let-values* ([(q-v env) (r-get q x y env)]
                                  [(U-v env) (u-get r-U x 0 env)]
                                  [(b-v env) (f-get r-b 0 y env)])
                      (y-loop (+ y 1)
                              (cons (make-qa0-operation attr*
                                      op-0
                                      (list (make-reg q-v))
                                      (list (make-reg U-v) (make-reg b-v)))
                                    r*)
                              env))]))]))))
        (define (s-madd-x r-x r-r c r* env)
          (let x-loop ([x 0] [r* r*] [env env])
            (cond
             [(= x c-n) (values r* env r-r)]
             [else
              (let y-loop ([y 0] [r* r*] [env env])
                (cond
                 [(= y f-n) (x-loop (+ x 1) r* env)]
                 [else
                  (let-values* ([(q-v env) (r-get r-x x y env)]
                                [(r-v env) (r-get r-r x y env)]
                                [(U-v env) (u-get r-U x c env)]
                                [(b-v env) (f-get r-b c y env)])
                    (y-loop (+ y 1)
                            (cons (make-qa0-operation attr*
                                    op-k
                                    (list (make-reg r-v))
                                    (list (make-reg q-v)
                                          (make-reg U-v)
                                          (make-reg b-v)))
                                  r*)
                            env))]))])))
        (if (= c-n 1) (s-mul-z r* env)
            (let-values* ([(r* env r-x) (s-mul-1 r* env)])
              (let loop ([c 1] [r* r*] [env env] [r-x r-x])
                (cond
                 [(= c (- c-n 1))
                   (let-values* ([(r* env r-x) (s-madd-x r-x r-r c r* env)])
                     (values r* env))]
                 [else
                   (let-values* ([(r* env r-x) (s-madd-x r-x (q2c-new-reg) c
                                                         r* env)])
                     (loop (+ c 1) r* env r-x))]))))))
    (define (q2c-mulu attr* output* input* a-get b-get op-0 op-k r* env)
      (q2c-mul-g attr* output* input* '*colors* q2c-get-gauge op-0 op-k
                 a-get b-get r* env))
    (define (q2c-get-clover r p a b c d env)
      (q2c-rename env r 'clover p a b c d))
    (define (q2c-get-transpose-clover r p a b c d env)
      (q2c-rename env r 'clover p c d a b))
    (define (q2c-get-fermion r i j env)
      (q2c-rename env r 'fermion i j))
    (define (q2c-get-projected-fermion r i j env)
      (q2c-rename env r 'projected-fermion i j))
    (define (q2c-get-staggered-fermion r i j env)
      (q2c-rename env r 'staggered-fermion i j))
    (define (q2c-get-gauge r i j env)
      (q2c-rename env r 'gauge i j))
    (define (q2c-get-conj-gauge r i j env)
      (q2c-rename env r 'gauge j i))
    (define (q2c-mulx attr* output* input* f-n f-get r* env)
      (q2c-mul-g attr* output* input* f-n f-get
                 'complex-mul 'complex-madd
                 q2c-get-gauge f-get r* env))
    (define (q2c-mulx-conj attr* output* input* f-n f-get r* env)
      (q2c-mul-g attr* output* input* f-n f-get
                 'complex-cmul 'complex-cmadd
                 q2c-get-conj-gauge f-get r* env))
    (define (q2c-check-list x* size msg)
      (if (not (= (length x*) size))
          (s-error "ERROR: ~a" msg)))
    (define (q2c-project attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma projection result")
      (q2c-check-list input* 1 "QCD gamma projection source")
      (let* ([kind (attr-lookup attr* 'project "qcd-project")]
             [op* (ce-lookup env (cons 'project kind)
                             "project op-table for ~a" kind)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [h-n (/ (ce-resolve-const env '*fermion-dim* "Fermion dim") 2)]
             [r-r (car output*)]
             [r-a (car input*)])
        (define (proj c h op r* env)
          (q2c-check-list op 4 "Projection operation")
          (let-values* ([op-0 (car op)] [(f-0) (cadr op)]
                        [(a-0 env) (q2c-rename env r-a 'fermion c f-0)]
                        [op-1 (caddr op)] [(f-1) (cadddr op)]
                        [(a-1 env) (q2c-rename env r-a 'fermion c f-1)]
                        [cmd (binary-cmd op-0 op-1)]
                        [(r env) (q2c-rename env r-r 'projected-fermion c h)])
            (values (cons (make-qa0-operation attr*
                            cmd
                            (list (make-reg r))
                            (list (make-reg a-0) (make-reg a-1)))
                          r*)
                    env)))
        (define (binary-cmd op-0 op-1)
          (case op-0
            [(plus-one)
             (case op-1
               [(plus-one) 'complex-add]
               [(minus-one) 'complex-sub]
               [(plus-i) 'complex-add-i]
               [(minus-i) 'complex-sub-i]
               [else (s-error "Unknown second factor (~a ~a)" op-0 op-1)])]
            [else (s-error "Unknown first factor (~a ~a)" op-0 op-1)]))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let h-loop ([h 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (proj c h (car op*) r* env)])
                            (h-loop (+ h 1) (cdr op*) r* env))]))]))))
    (define (q2c-unproject attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma unprojection result")
      (q2c-check-list input* 1 "QCD gamma unprojection source")
      (let* ([kind (attr-lookup attr* 'unproject "qcd-unproject")]
             [op* (ce-lookup env (cons 'unproject kind)
                             "unproj op-table for ~a" kind)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [f-n (ce-resolve-const env '*fermion-dim* "Fermion dim")]
             [r-r (car output*)]
             [r-a (car input*)])
        (define (unproj c f op r* env)
          (q2c-check-list op 2 "Unprojection operation")
          (let-values* ([opcode (car op)] [(component) (cadr op)]
                        [(a env) (q2c-rename env r-a
                                             'projected-fermion c component)]
                        [cmd (unary-cmd opcode)]
                        [(r env) (q2c-rename env r-r 'fermion c f)])
            (values (cons (make-qa0-operation attr*
                            cmd (list (make-reg r)) (list (make-reg a)))
                          r*)
                    env)))
        (define (unary-cmd name)
          (case name
            [(plus-one)   'complex-move]
            [(minus-one)  'complex-neg]
            [(plus-i)     'complex-times-plus-i]
            [(minus-i)    'complex-times-minus-i]
            [else (s-error "Unknown unproject operation ~a" name)]))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (unproj c f (car op*) r* env)])
                            (f-loop (+ f 1) (cdr op*) r* env))]))]))))

    (define (q2c-unproject-add attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma unproject-add result")
      (q2c-check-list input* 2 "QCD gamma unproject-add sources")
      (let* ([kind (attr-lookup attr* 'unproject "qcd-unproject-add")]
             [op* (ce-lookup env (cons 'unproject kind)
                             "unproj op-table for ~a" kind)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [r-r (car output*)]
             [r-a (car input*)]
             [r-b (cadr input*)])
        (define (upa c f op r* env)
          (q2c-check-list op 2 "Unprojection operation")
          (let-values* ([(a env) (q2c-rename env r-a 'fermion c f)]
                        [opcode (car op)] [(component) (cadr op)]
                        [(b env) (q2c-rename env r-b 'projected-fermion
                                             c component)]
                        [cmd (add-cmd opcode)]
                        [(r env) (q2c-rename env r-r 'fermion c f)])
            (values (cons (make-qa0-operation attr*
                            cmd
                            (list (make-reg r))
                            (list (make-reg a) (make-reg b)))
                          r*)
                    env)))
        (define (add-cmd name)
          (case name
            [(plus-one)   'complex-add]
            [(minus-one)  'complex-sub]
            [(plus-i)     'complex-add-i]
            [(minus-i)    'complex-sub-i]
            [else (s-error "Unknown unproject operation ~a" name)]))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (upa c f (car op*) r* env)])
                            (f-loop (+ f 1) (cdr op*) r* env))]))]))))
    (define (q2c-unproject-sub attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma unproject-sub result")
      (q2c-check-list input* 2 "QCD gamma unproject-sub sources")
      (let* ([kind (attr-lookup attr* 'unproject "qcd-unproject-sub")]
             [op* (ce-lookup env (cons 'unproject kind)
                             "unproj op-table for ~a" kind)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [r-r (car output*)]
             [r-a (car input*)]
             [r-b (cadr input*)])
        (define (upa c f op r* env)
          (q2c-check-list op 2 "Unprojection operation")
          (let-values* ([(a env) (q2c-rename env r-a 'fermion c f)]
                        [opcode (car op)] [(component) (cadr op)]
                        [(b env) (q2c-rename env r-b 'projected-fermion
                                             c component)]
                        [cmd (sub-cmd opcode)]
                        [(r env) (q2c-rename env r-r 'fermion c f)])
            (values (cons (make-qa0-operation attr*
                            cmd
                            (list (make-reg r))
                            (list (make-reg a) (make-reg b)))
                          r*)
                    env)))
        (define (sub-cmd name)
          (case name
            [(plus-one)   'complex-sub]
            [(minus-one)  'complex-add]
            [(plus-i)     'complex-sub-i]
            [(minus-i)    'complex-add-i]
            [else (s-error "Unknown unproject operation ~a" name)]))
        (let c-loop ([c 0] [r* r*] [env env])
          (cond
           [(= c c-n) (values r* env)]
           [else (let f-loop ([f 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (upa c f (car op*) r* env)])
                            (f-loop (+ f 1) (cdr op*) r* env))]))]))))
    (define (q2c-unproject-5-dot attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma unproject-5-dot result")
      (q2c-check-list input* 2 "QCD gamma unproject-5-dot sources")
      (let* ([kind (attr-lookup attr* 'unproject-5 "qcd-unproject-5-dot")]
             [op* (ce-lookup env (cons 'unproject-5 kind)
                             "xxx unproj op-table for ~a" kind)]
             [dot-r (q2c-new-reg)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [r-r (car output*)]
             [r-a (car input*)]
             [r-b (cadr input*)])
        (define (upa c f op r* env)
          (q2c-check-list op 2 "Unprojection operation")
          (let-values* ([(b env) (q2c-rename env r-b 'fermion c f)]
                        [opcode (car op)] [(component) (cadr op)]
                        [(a env) (q2c-rename env r-a 'projected-fermion
                                             c component)]
                        [cmd (add-cmd opcode)])
            (values (cons (make-qa0-operation attr*
                            cmd
                            (list (make-reg dot-r))
                            (list (make-reg dot-r) (make-reg a) (make-reg b)))
                          r*)
                    env)))
        (define (add-cmd name)
          (case name
            [(plus-one)   'complex-dot-add]
            [(minus-one)  'complex-dot-sub]
            [(plus-i)     'complex-dot-sub-i]
            [(minus-i)    'complex-dot-add-i]
            [else (s-error "Unknown unproject operation ~a" name)]))
        (define (start-op r*)
          (cons (make-qa0-operation attr*
                                    'complex-dot-init
                                    (list (make-reg dot-r))
                                    '())
                r*))
        (define (finish-op r*)
          (cons (make-qa0-operation attr*
                                    'complex-dot-fini
                                    (list r-r)
                                    (list (make-reg dot-r)))
                r*))
        (let c-loop ([c 0] [r* (start-op r*)] [env env])
          (cond
           [(= c c-n) (values (finish-op r*) env)]
           [else (let f-loop ([f 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (upa c f (car op*) r* env)])
                            (f-loop (+ f 1) (cdr op*) r* env))]))]))))
;;---
    (define (q2c-dot-unproject-5 attr* output* input* r* env)
      (q2c-check-list output* 1 "QCD gamma dot-unproject-5 result")
      (q2c-check-list input* 2 "QCD gamma dot-unproject-5 sources")
      (let* ([kind (attr-lookup attr* 'unproject-5 "qcd-dot-unproject-5")]
             [op* (ce-lookup env (cons 'unproject-5 kind)
                             "unproj op-table for ~a" kind)]
             [c-n (ce-resolve-const env '*colors* "Color count")]
             [dot-r (q2c-new-reg)]
             [r-r (car output*)]
             [r-a (car input*)]
             [r-b (cadr input*)])
        (define (upa c f op r* env)
          (q2c-check-list op 2 "Unprojection operation")
          (let-values* ([(a env) (q2c-rename env r-a 'fermion c f)]
                        [opcode (car op)] [(component) (cadr op)]
                        [(b env) (q2c-rename env r-b 'projected-fermion
                                             c component)]
                        [cmd (add-cmd opcode)])
            (values (cons (make-qa0-operation attr*
                            cmd
                            (list (make-reg dot-r))
                            (list (make-reg dot-r) (make-reg a) (make-reg b)))
                          r*)
                    env)))
        (define (add-cmd name)
          (case name
            [(plus-one)   'complex-dot-add]
            [(minus-one)  'complex-dot-sub]
            [(plus-i)     'complex-dot-add-i]
            [(minus-i)    'complex-dot-sub-i]
            [else (s-error "Unknown unproject operation ~a" name)]))
        (define (start-op r*)
          (cons (make-qa0-operation attr*
                                    'complex-dot-init
                                    (list (make-reg dot-r))
                                    '())
                r*))
        (define (finish-op r*)
          (cons (make-qa0-operation attr*
                                    'complex-dot-fini
                                    (list r-r)
                                    (list (make-reg dot-r)))
                r*))
        (let c-loop ([c 0] [r* (start-op r*)] [env env])
          (cond
           [(= c c-n) (values (finish-op r*) env)]
           [else (let f-loop ([f 0] [op* op*] [r* r*] [env env])
                   (cond
                    [(null? op*) (c-loop (+ c 1) r* env)]
                    [else (let-values* ([(r* env) (upa c f (car op*) r* env)])
                            (f-loop (+ f 1) (cdr op*) r* env))]))]))))
    (define q2c-op*
      (list
       (cons 'qcd-project                   q2c-project)
       (cons 'qcd-unproject                 q2c-unproject)
       (cons 'qcd-unproject-add             q2c-unproject-add)
       (cons 'qcd-unproject-sub             q2c-unproject-sub)
       (cons 'qcd-unproject-5-dot           q2c-unproject-5-dot)
       (cons 'qcd-dot-unproject-5           q2c-dot-unproject-5)
       (cons 'qcd-mulf                      q2c-mulf)
       (cons 'qcd-mulh                      q2c-mulh)
       (cons 'qcd-muls                      q2c-muls)
       (cons 'qcd-mulf-conj                 q2c-mulf-conj)
       (cons 'qcd-mulh-conj                 q2c-mulh-conj)
       (cons 'qcd-muls-conj                 q2c-muls-conj)
       (cons 'qcd-mul-clover                q2c-mul-clover)
       (cons 'qcd-conj-mul-clover           q2c-conj-mul-clover)
       (cons 'qcd-su-n-mul                  q2c-u-mul)
       (cons 'qcd-su-n-conj-mul             q2c-u-conj-mul)
       (cons 'qcd-su-n-mul-conj             q2c-u-mul-conj)
       (cons 'qcd-su-n-conj-mul-conj        q2c-u-conj-mul-conj)
       (cons 'qcd-su-n-real-trace-conj-mul  q2c-u-retr-conj-mul)
       (cons 'qcd-zerou                     q2c-zerou)
       (cons 'qcd-zerof                     q2c-zerof)
       (cons 'qcd-zeroh                     q2c-zeroh)
       (cons 'qcd-zeros                     q2c-zeros)
       (cons 'qcd-scaleu                    q2c-scaleu)
       (cons 'qcd-scalef                    q2c-scalef)
       (cons 'qcd-scalef-lo                 q2c-scalef-lo)
       (cons 'qcd-scalef-hi                 q2c-scalef-hi)
       (cons 'qcd-scaleh                    q2c-scaleh)
       (cons 'qcd-scales                    q2c-scales)
       (cons 'qcd-complex-scaleu            q2c-complex-scaleu)
       (cons 'qcd-complex-scalef            q2c-complex-scalef)
       (cons 'qcd-complex-scalef-lo         q2c-complex-scalef-lo)
       (cons 'qcd-complex-scalef-hi         q2c-complex-scalef-hi)
       (cons 'qcd-complex-scaleh            q2c-complex-scaleh)
       (cons 'qcd-complex-scales            q2c-complex-scales)
       (cons 'qcd-complex-conj-scaleu       q2c-complex-conj-scaleu)
       (cons 'qcd-complex-conj-scalef       q2c-complex-conj-scalef)
       (cons 'qcd-complex-conj-scalef-lo    q2c-complex-conj-scalef-lo)
       (cons 'qcd-complex-conj-scalef-hi    q2c-complex-conj-scalef-hi)
       (cons 'qcd-complex-conj-scaleh       q2c-complex-conj-scaleh)
       (cons 'qcd-complex-conj-scales       q2c-complex-conj-scales)
       (cons 'qcd-addu                      q2c-addu)
       (cons 'qcd-addf                      q2c-addf)
       (cons 'qcd-addh                      q2c-addh)
       (cons 'qcd-adds                      q2c-adds)
       (cons 'qcd-subu                      q2c-subu)
       (cons 'qcd-subf                      q2c-subf)
       (cons 'qcd-subh                      q2c-subh)
       (cons 'qcd-subs                      q2c-subs)
       (cons 'qcd-cmaddf                    q2c-cmaddf)
       (cons 'qcd-cmaddf-lo                 q2c-cmaddf-lo)
       (cons 'qcd-cmaddf-hi                 q2c-cmaddf-hi)
       (cons 'qcd-cmaddh                    q2c-cmaddh)
       (cons 'qcd-cmadds                    q2c-cmadds)
       (cons 'qcd-maddf                     q2c-maddf)
       (cons 'qcd-maddf-lo                  q2c-maddf-lo)
       (cons 'qcd-maddf-hi                  q2c-maddf-hi)
       (cons 'qcd-madd-lohi                 q2c-madd-lohi)
       (cons 'qcd-maddh                     q2c-maddh)
       (cons 'qcd-madds                     q2c-madds)
       (cons 'qcd-msubf                     q2c-msubf)
       (cons 'qcd-msubf-lo                  q2c-msubf-lo)
       (cons 'qcd-msubf-hi                  q2c-msubf-hi)
       (cons 'qcd-msubh                     q2c-msubh)
       (cons 'qcd-msubs                     q2c-msubs)
       (cons 'qcd-msubf                     q2c-msubf)
       (cons 'qcd-cmsubf                    q2c-cmsubf)
       (cons 'qcd-cmsubf-lo                 q2c-cmsubf-lo)
       (cons 'qcd-cmsubf-hi                 q2c-cmsubf-hi)
       (cons 'qcd-fnorm-init                q2c-fnorm-init)
       (cons 'qcd-fnorm-add                 q2c-fnorm-add)
       (cons 'qcd-fnorm-lo-add              q2c-fnorm-lo-add)
       (cons 'qcd-fnorm-hi-add              q2c-fnorm-hi-add)
       (cons 'qcd-fnorm-fini                q2c-fnorm-fini)
       (cons 'qcd-snorm-init                q2c-snorm-init)
       (cons 'qcd-snorm-add                 q2c-snorm-add)
       (cons 'qcd-snorm-fini                q2c-snorm-fini)
       (cons 'qcd-fdot-init                 q2c-fdot-init)
       (cons 'qcd-fdot-add                  q2c-fdot-add)
       (cons 'qcd-fdot-fini                 q2c-fdot-fini)
       (cons 'qcd-sdot-init                 q2c-sdot-init)
       (cons 'qcd-sdot-add                  q2c-sdot-add)
       (cons 'qcd-sdot-fini                 q2c-sdot-fini)
       (cons 'qcd-su-n-offset               q2c-su-n-offset)
       (cons 'qcd-su-n-double-offset        q2c-su-n-double-offset)
       (cons 'qcd-su-n-float-offset         q2c-su-n-float-offset)
       (cons 'qcd-fermion-offset            q2c-fermion-offset)
       (cons 'qcd-fermion-double-offset     q2c-fermion-double-offset)
       (cons 'qcd-fermion-float-offset      q2c-fermion-float-offset)
       (cons 'qcd-clover-lo-offset          q2c-clover-lo-offset)
       (cons 'qcd-clover-lo-double-offset   q2c-clover-lo-double-offset)
       (cons 'qcd-clover-lo-float-offset    q2c-clover-lo-float-offset)
       (cons 'qcd-clover-hi-offset          q2c-clover-hi-offset)
       (cons 'qcd-clover-hi-double-offset   q2c-clover-hi-double-offset)
       (cons 'qcd-clover-hi-float-offset    q2c-clover-hi-float-offset)
       (cons 'qcd-staggered-fermion-offset  q2c-staggered-fermion-offset)
       (cons 'qcd-staggered-fermion-double-offset
             q2c-staggered-fermion-double-offset)
       (cons 'qcd-staggered-fermion-float-offset
             q2c-staggered-fermion-float-offset)
       (cons 'qcd-projected-fermion-offset  q2c-projected-fermion-offset)
       (cons 'qcd-projected-fermion-double-offset
             q2c-projected-fermion-double-offset)
       (cons 'qcd-projected-fermion-float-offset
             q2c-projected-fermion-float-offset)))
    (define (q2c-operation c attr* name output* input* r* env)
      (cond
       [(assq name q2c-op*)
        => (lambda (n&f) ((cdr n&f) attr* output* input* r* env))]
       [else (values (cons c r*) env)]))

    (lambda (ast env)
      (variant-case ast
        [qa0-top (decl*)
          (let loop ([r* '()] [decl* decl*] [env env])
            (cond
             [(null? decl*) (values (make-qa0-top (reverse r*)) env)]
             [else (let-values* ([(d e) (q2c-decl (car decl*) env)])
                     (loop (cons d r*) (cdr decl*) e))]))]))))
