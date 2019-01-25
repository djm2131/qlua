;; BG/Q XLC backend
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "format.ss"
#fload "ast.ss"
#fload "parser.ss"
#fload "attr.ss"
#fload "backend.ss"
#fload "cx2qh.ss"
#fload "cenv.ss"
#fload "cheader.ss"
#fload "verbose.ss"
#fload "be-ckind.ss"
;;
;; (provide machine-bgq/xlc)
;;
(define machine-bgq/xlc
  (let ()
    (define op-type-table
      '((qh-make                   qh-double)
        (qh-real                   double)
        (qh-imag                   double)
        (qh-zero                   qh-double)
        (qh-move                   qh-double)
        (qh-neg                    qh-double)
        (qh-times-plus-i           qh-double)
        (qh-times-minus-i          qh-double)
        (qh-add                    qh-double)
        (qh-sub                    qh-double)
        (qh-rmul                   qh-double)
        (qh-rmadd                  qh-double)
        (qh-rmsub                  qh-double)
        (qh-add-i                  qh-double)
        (qh-sub-i                  qh-double)
        (qh-norm-init              qh-double)
        (qh-norm-add               qh-double)
        (qh-norm-fini              double)
        (qh-dot-init               qh-double)
        (qh-dot-add-a              qh-double)
        (qh-dot-add-b              qh-double)
        (qh-dot-add-i-a            qh-double)
        (qh-dot-add-i-b            qh-double)
        (qh-dot-sub-a              qh-double)
        (qh-dot-sub-b              qh-double)
        (qh-dot-sub-i-a            qh-double)
        (qh-dot-sub-i-b            qh-double)
        (qh-dot-fini               qh-double)
        (qh-mul-a                  qh-double)
        (qh-mul-b                  qh-double)
        (qh-madd-a                 qh-double)
        (qh-madd-b                 qh-double)
        (qh-msub-a                 qh-double)
        (qh-msub-b                 qh-double)
        (qh-cmul-a                 qh-double)
        (qh-cmul-b                 qh-double)
        (qh-cmadd-a                qh-double)
        (qh-cmadd-b                qh-double)
        (qh-cmsub-a                qh-double)
        (qh-cmsub-b                qh-double)
        (qh->float                 qh-double)
        (double-add                double)
        (double-div                double)
        (double-madd               double)
        (double-msub               double)
        (double-zero               double)
        (double-move               double)
        (double-mul                double)
        (double-neg                double)
        (double-sub                double)
        (int-add                   int)
        (int-and                   int)
        (int-div                   int)
        (int-mod                   int)
        (int-move                  int)
        (int-mul                   int)
        (int-or                    int)
        (int-sub                   int)
        (int-xor                   int)
        (pointer-add               pointer)
        (pointer-move              pointer)
        (nop                       )))
    (define op-emit-table
      ;; op i-count fmt flop-count
      '((pointer-move            1 "$0 = %0"                                                                 0)
        (pointer-add             2 "$0 = %0 + (%1)"                                                          0)
        (int-move                1 "$0 = %0"                                                                 0)
        (int-mul                 2 "$0 = %0 * (%1)"                                                          0)
        (int-div                 2 "$0 = %0 / (%1)"                                                          0)
        (int-mod                 2 "$0 = %0 % (%1)"                                                          0)
        (int-add                 2 "$0 = %0 + (%1)"                                                          0)
        (int-sub                 2 "$0 = %0 - (%1)"                                                          0)
        (int-neg                 1 "$0 = -(%0)"                                                              0)
        (int-and                 2 "$0 = %0 & (%1)"                                                          0)
        (int-or                  2 "$0 = %0 | (%1)"                                                          0)
        (int-xor                 2 "$0 = %0 ^ (%1)"                                                          0)
        (int-not                 1 "$0 = !(%0)"                                                              0)
        (double-zero             0 "$0 = 0.0"                                                                0)
        (double-move             1 "$0 = %0"                                                                 0)
        (double-neg              1 "$0 = - (%0)"                                                             1)
        (double-add              2 "$0 = %0 + (%1)"                                                          1)
        (double-sub              2 "$0 = %0 - (%1)"                                                          1)
        (double-div              2 "$0 = %0 / (%1)"                                                          1)
        (double-mul              2 "$0 = %0 * (%1)"                                                          1)
        (double-madd             3 "$0 = %0 + (%1) * (%2)"                                                   2)
        (double-msub             3 "$0 = %0 - (%1) * (%2)"                                                   2)
        (qh-make                 2 "$0 = vec_insert(%1,vec_promote(%0,0),1)"                                 0)
        (qh-real                 1 "$0 = vec_extract(%0,0)"                                                  0)
        (qh-imag                 1 "$0 = vec_extract(%0,1)"                                                  0)
        (qh-zero                 0 "$0 = gZERO"                                                              0)
        (qh-move                 1 "$0 = %0"                                                                 0)
        (qh-neg                  1 "$0 = vec_neg(%0)"                                                        2)
        (qh-times-plus-i         1 "$0 = vec_xxnpmadd(%0, gONE, gZERO)"                                      1)
        (qh-times-minus-i        1 "$0 = vec_xxcpnmadd(%0, gONE, gZERO)"                                     1)
        (qh-add                  2 "$0 = vec_add(%0, %1)"                                                    2)
        (qh-sub                  2 "$0 = vec_sub(%0, %1)"                                                    2)
        (qh-rmul                 2 "$0 = vec_mul(vec_splats(%0), %1)"                                        2)
        (qh-rmadd                3 "$0 = vec_madd(vec_splats(%1), %2, %0)"                                   4)
        (qh-rmsub                3 "$0 = vec_nmsub(vec_splats(%1), %2, %0)"                                  4)
        (qh-add-i                2 "$0 = vec_xxnpmadd(%1, gONE, %0)"                                         2)
        (qh-sub-i                2 "$0 = vec_xxcpnmadd(%1, gONE, %0)"                                        2)
        (qh-mul-a                2 "$0 = vec_xmul(%0, %1)"                                                   2)
        (qh-mul-b                3 "$0 = vec_xxnpmadd(%2, %1, %0)"                                           4)
        (qh-madd-a               3 "$0 = vec_xmadd(%1, %2, %0)"                                              4)
        (qh-madd-b               3 "$0 = vec_xxnpmadd(%2, %1, %0)"                                           4)
        (qh-msub-a               3 "$0 = vec_sub(%0, vec_xmul(%1, %2))"                                      4)
        (qh-msub-b               3 "$0 = vec_xxcpnmadd(%2, %1, %0)"                                          4)
        (qh-cmul-a               2 "$0 = vec_xmul(%0, %1)"                                                   2)
        (qh-cmul-b               3 "$0 = vec_xxcpnmadd(%2, %1, %0)"                                          4)
        (qh-cmadd-a              3 "$0 = vec_xmadd(%1, %2, %0)"                                              4)
        (qh-cmadd-b              3 "$0 = vec_xxcpnmadd(%2, %1, %0)"                                          4)
        (qh-cmsub-a              3 "$0 = vec_sub(%0, vec_xmul(%1, %2))"                                      4)
        (qh-cmsub-b              3 "$0 = vec_xxnpmadd(%2, %1, %0)"                                           4)
        (qh-real-cmul-conj-init  0 "$0 = gZERO"                                                              0)
        (qh-real-cmul-conj-add   3 "$0 = vec_madd(%2, %1, %0)"                                               4)
        (qh-real-cmul-conj-fini  1 "$0 = vec_extract(%0,0) + vec_extract(%0,1)"                              1)
        (qh-norm-init            0 "$0 = gZERO"                                                              0)
        (qh-norm-add             2 "$0 = vec_madd(%1, %1, %0)"                                               4)
        (qh-norm-fini            1 "$0 = vec_extract(%0,0) + vec_extract(%0,1)"                              1)
        (qh-dot-init             0 "$0 = gZERO"                                                              0)
        (qh-dot-add-a            3 "$0 = vec_xmadd(%1, %2, %0)"                                              4)
        (qh-dot-add-b            3 "$0 = vec_xxcpnmadd(%2, %1, %0)"                                          4)
        (qh-dot-add-i-a          3 "$0 = vec_madd(vec_splats(vec_extract(%1,1)),%2,%0)"                      4)
        (qh-dot-add-i-b          3 "$0 = vec_xxnpmadd(%2,vec_splats(vec_extract(%1,0)),%0)"                  4)
        (qh-dot-sub-a            3 "$0 = vec_sub(%0, vec_xmul(%1, %2))"                                      4)
        (qh-dot-sub-b            3 "$0 = vec_xxnpmadd(%2, %1, %0)"                                           4)
        (qh-dot-sub-i-a          3 "$0 = vec_msub(vec_splats(vec_extract(%1,1)),%2,%0)"                      4)
        (qh-dot-sub-i-b          3 "$0 = vec_xxcpnmadd(%2,vec_splats(vec_extract(%1,0)),%0)"                 4)
        (qh-dot-fini             1 "$0 = %0"                                                                 0)
        (qh->float               1 "$0 = vec_rsp(%0)"                                                        0)))
    (define load-table
      '((int               int)
        (pointer           pointer)
        (float             double)
        (double            double)
        (qh-float          qh-double)
        (qh-double         qh-double)))
    (define op-needing-zero*
      '(qh-zero
        qh-times-plus-i
        qh-times-minus-i
        qh-real-cmul-conj-init
        qh-norm-init
        qh-dot-init
        qh-dot-add-i-b
        qh-dot-sub-i-b))
    (define op-needing-one*
      '(qh-times-plus-i
        qh-times-minus-i
        qh-add-i
        qh-sub-i
        qh-dot-add-i-a
        qh-dot-add-i-b
        qh-dot-sub-i-a
        qh-dot-sub-i-b))
    (define (emit-qh-double-store level addr* value env)
      (do-emit level (q-fmt "vec_st2a(~a, 0, (double *)(~a));"
                            (preemit-input value env)
                            (preemit-addr* addr* env))))
    (define (emit-qh-float-store level addr* value env)
      (do-emit level (q-fmt "vec_st2a(~a, 0, (float *)(~a));"
                            (preemit-input value env)
                            (preemit-addr* addr* env))))
    (define (emit-qh-double-load level output addr* env)
      (do-emit level (q-fmt "~a = vec_ld2a(0, (double *)(~a));"
                            (preemit-output output env)
                            (preemit-addr* addr* env))))
    (define (emit-qh-float-load level output addr* env)
      (do-emit level (q-fmt "~a = vec_ld2a(0, (float *)(~a));"
                            (preemit-output output env)
                            (preemit-addr* addr* env))))
    (define (emit-load level type output addr* env f)
      (case type
        [(qh-float) (emit-qh-float-load level output addr* env)]
        [(qh-double) (emit-qh-double-load level output addr* env)]
        [else (do-emit level "~a = *(~a *)(~a);"
                       (preemit-output output env)
                       (ce-lookup-x env 'name-of type "BGQ/XLC name of type ~a"
                                    type)
                       (preemit-addr* addr* env))])
      f)
    (define (emit-store level type addr* value env f)
      (case type
        [(qh-float) (emit-qh-float-store level addr* value env)]
        [(qh-double) (emit-qh-double-store level addr* value env)]
        [else (do-emit level "*(~a *)(~a) = ~a;"
                       (ce-lookup-x env 'name-of type "BGQ/XLC name of type ~a"
                                    type)
                       (preemit-addr* addr* env)
                       (preemit-input value env))])
      f)
    (define (collect-output* code* env)
      (define (add-output* output* type* env)
        (cond
         [(null? output*) env]
         [else (add-output* (cdr output*) (cdr type*)
                            (add-output (car output*) (car type*) env))]))
      (define (add-output output type env)
        (variant-case output
          [reg (name)
            (ce-search-x env 'back-end name
                         (lambda (v) env)
                         (lambda () (ce-bind-x env 'back-end name
                                               (list (ck-new-var) type))))]))
      (define (add-zero name env)
        (if (not (memq name op-needing-zero*)) env
            (ce-search-x env 'bgq/xlc 'zero
                         (lambda (x) env)
                         (lambda ()
                           (ce-bind-x env 'bgq/xlc 'zero (ck-new-var))))))
      (define (add-one name env)
        (if (not (memq name op-needing-one*)) env
            (ce-search-x env 'bgq/xlc 'one
                         (lambda (x) env)
                         (lambda ()
                           (ce-bind-x env 'bgq/xlc 'one (ck-new-var))))))
      (walk-code* code*
                  ;; operation
                  (lambda (env name attr* output* input*)
                    (let* ([env (add-zero name env)]
                           [env (add-one name env)])
                      (add-output* output* (be-out-type* env name) env)))
                  ;; load
                  (lambda (env type attr* output addr*)
                    (add-output output (be-load-type env type) env))
                  ;; store
                  (lambda (env type attr* addr* value) env)
                  ;; loop
                  (lambda (env attr* var low high)
                    (add-output var 'int env))
                  ;; if
                  (lambda (env var) env)
                  env))
    (define (bgq/xlc-back-end ast env)
      (values (complex->quad-hummer ast) env))
    (define (extra-env env)
      (let* ([env (ce-add-type env 'qh-double   "vector4double" 32 32)]
             [env (ce-add-type env 'qh-float    "vector4double" 32 32)])
        env))
    (define (extra-decl* arg-name* arg-type* arg-c-name* arg-c-type* env)
      (define (do-constant id init)
        (ce-search-x env 'bgq/xlc id
                     (lambda (v) (do-emit 1 "const vector4double ~a = ~a;"
                                          v init))
                     (lambda () #t)))
      (do-constant 'zero "((vector4double) {0.0, 0.0, 0.0, 0.0})")
      (do-constant 'one  "((vector4double) {1.0, 1.0, 1.0, 1.0})"))

    (define (zo-define* env)
      (define (do-define id name def)
        (ce-search-x env 'bgq/xlc id
                     (lambda (v)
                       (do-emit 0 "#define ~a ~a" name (q-fmt def v)))
                     (lambda () #t)))
       (do-define 'zero "gZERO" "(~a)")
       (do-define 'one "gONE" "(~a)"))
    (define (extra-postparam* arg-name* arg-type* arg-c-name* arg-c-type* env)
      (define (emit-disjoint*)
        (do-emit 0 "")
        (let loop-a ([a* arg-name*] [t* arg-type*])
          (cond
           [(null? a*)]
           [(not (eq? 'pointer (car t*))) (loop-a (cdr a*) (cdr t*))]
           [else
            (let ([rn (car (ce-lookup-x env 'back-end (car a*)
                                        "bgq/xlc name for ~a" (car a*)))])
              (let loop-b ([b* (cdr a*)] [s* (cdr t*)])
                (cond
                 [(null? b*) (loop-a (cdr a*) (cdr t*))]
                 [(not (eq? 'pointer (car s*))) (loop-b (cdr b*) (cdr s*))]
                 [else (do-emit 0 "#pragma disjoint(*~a,*~a)"
                                rn
                                (car (ce-lookup-x env 'back-end (car b*)
                                                  "bgq/xlc name for ~a"
                                                  (car b*))))
                       (loop-b (cdr b*) (cdr s*))])))])))
      (define (emit-align*)
        (do-emit 0 "")
        (let loop-a ([a* arg-name*] [t* arg-type*])
          (cond
           [(null? a*) (do-emit 0 "")]
           [(not (eq? 'pointer (car t*))) (loop-a (cdr a*) (cdr t*))]
           [else
            (let ([rn (car (ce-lookup-x env 'back-end (car a*)
                                        "bgq/xlc name for ~a" (car a*)))])
              (do-emit 1 "__alignx(16, ~a);" rn)
              (loop-a (cdr a*) (cdr t*)))])))
      ;;;
      (emit-disjoint*)
      (emit-align*))
    (define (zo-undefine* env)
      (define (do-undefine id name)
        (ce-search-x env 'bgq/xlc id
                     (lambda (v) (do-emit 0 "#undef ~a" name))
                     (lambda () #t)))
      (do-undefine 'zero "gZERO")
      (do-undefine 'one "gONE"))

    (build-ckind-back-end 'bgq/xlc          ; target-name
                          8                 ; int-size
                          8                 ; int-align
                          8                 ; pointer-size
                          8                 ; pointer-align
                          op-emit-table     ; op-emit-table
                          op-type-table     ; op-type-table
                          load-table        ; ld-type-table
                          emit-load         ; emit-load
                          emit-store        ; emit-store
                          collect-output*   ; collect-outputs
                          bgq/xlc-back-end  ; the-back-end
                          extra-env         ; extra-env
                          extra-decl*       ; extra-decl*
                          zo-define*        ; extra-def*
                          extra-postparam*  ; extra-postparam*
                          zo-undefine*)))   ; extra-undef*
