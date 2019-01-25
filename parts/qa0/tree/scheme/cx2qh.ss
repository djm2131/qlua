;; Complex to Quad Hummer converter
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "ast.ss"
#fload "split.ss"
;;
;; (provide complex->quad-hummer)
;;
(define complex->quad-hummer
  (let ()
    (define (cx->qh-decl decl)
      (variant-case decl
        [qa0-proc (attr* name arg-name* arg-type* arg-c-name* arg-c-type* code*)
          (cx->qh-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type* 
                       code*)]
        [else decl]))
    (define (cx->qh-proc attr* name
                         arg-name* arg-type*
                         arg-c-name* arg-c-type*
                         code*)
      (make-qa0-proc attr* name
                     arg-name* arg-type*
                     arg-c-name* arg-c-type*
                     (cx->qh-code* code*)))
    (define (cx->qh-code* code*)
      (let loop ([code* code*] [r* '()])
        (cond
         [(null? code*) (reverse r*)]
         [else (loop (cdr code*) (cx->qh-code (car code*) r*))])))
    (define (cx->qh-code code r*)
      (variant-case code
        [qa0-operation (attr* name output* input*)
          (cx->qh-op code attr* name output* input* r*)]
        [qa0-load (attr* type output addr*)
          (cx->qh-load attr* type output addr* r*)]
        [qa0-store (attr* type addr* value)
          (cx->qh-store attr* type addr* value r*)]
        [qa0-loop (attr* var low high code*)
          (cx->qh-loop attr* var low high code* r*)]
        [qa0-if (var true-code* false-code*)
          (cx->qh-if var true-code* false-code* r*)]))
    (define qh-optable
      (list
       (cons 'complex-zero                 (/-rename 1 0 'qh-zero))
       (cons 'complex-move                 (/-rename 1 1 'qh-move))
       (cons 'complex                      (/-rename 1 2 'qh-make))
       (cons 'complex-real                 (/-rename 1 1 'qh-real))
       (cons 'complex-imag                 (/-rename 1 1 'qh-imag))
       (cons 'complex-neg                  (/-rename 1 1 'qh-neg))
       (cons 'complex-times-plus-i         (/-rename 1 1 'qh-times-plus-i))
       (cons 'complex-times-minus-i        (/-rename 1 1 'qh-times-minus-i))
       (cons 'complex-add                  (/-rename 1 2 'qh-add))
       (cons 'complex-sub                  (/-rename 1 2 'qh-sub))
       (cons 'complex-rmul                 (/-rename 1 2 'qh-rmul))
       (cons 'complex-mul                  (/-split2 1 2   'qh-mul-a       /in-all  'qh-mul-b))
       (cons 'complex-madd                 (/-split2 1 3   'qh-madd-a      /in-tail 'qh-madd-b))
       (cons 'complex-msub                 (/-split2 1 3   'qh-msub-a      /in-tail 'qh-msub-b))
       (cons 'complex-rmadd                (/-rename 1 3 'qh-rmadd))
       (cons 'complex-rmsub                (/-rename 1 3 'qh-rmsub))
       (cons 'complex-cmul                 (/-split2 1 2   'qh-cmul-a      /in-all  'qh-cmul-b))
       (cons 'complex-cmadd                (/-split2 1 3   'qh-cmadd-a     /in-tail 'qh-cmadd-b))
       (cons 'complex-cmsub                (/-split2 1 3   'qh-cmsub-a     /in-tail 'qh-cmsub-b))
       (cons 'complex-add-i                (/-rename 1 2 'qh-add-i))
       (cons 'complex-sub-i                (/-rename 1 2 'qh-sub-i))
       (cons 'complex-real-cmul-conj-init  (/-rename 1 2 'qh-real-cmul-conj-init))
       (cons 'complex-real-cmul-conj-add   (/-rename 1 3 'qh-real-cmul-conj-add))
       (cons 'complex-real-cmul-conj-fini  (/-rename 1 1 'qh-real-cmul-conj-fini))
       (cons 'complex-norm-init            (/-rename 1 0 'qh-norm-init))
       (cons 'complex-norm-add             (/-rename 1 2 'qh-norm-add))
       (cons 'complex-norm-fini            (/-rename 1 1 'qh-norm-fini))
       (cons 'complex-dot-init             (/-rename 1 0 'qh-dot-init))
       (cons 'complex-dot-add              (/-split2 1 3   'qh-dot-add-a   /in-tail 'qh-dot-add-b))
       (cons 'complex-dot-add-i            (/-split2 1 3   'qh-dot-add-i-a /in-tail 'qh-dot-add-i-b))
       (cons 'complex-dot-sub              (/-split2 1 3   'qh-dot-sub-a   /in-tail 'qh-dot-sub-b))
       (cons 'complex-dot-sub-i            (/-split2 1 3   'qh-dot-sub-i-a /in-tail 'qh-dot-sub-i-b))
       (cons 'complex-dot-fini             (/-rename 1 1 'qh-dot-fini))))
    (define (cx->qh-op code attr* name output* input* r*)
      (let ([trans (assq name qh-optable)])
        (cond
         [trans ((cdr trans) name attr* output* input* r*)]
         [else (cons code r*)])))
    (define (cx->qh-load attr* type output addr* r*)
      (cons (make-qa0-load attr* (convert-type type) output addr*)
            r*))
    (define (qho-store-float attr* addr* value r*)
      (let ([x (/-fresh-reg)])
        (list* (make-qa0-store attr* 'qh-float addr* x)
               (make-qa0-operation '() 'qh->float (list x) (list value))
               r*)))
    (define (cx->qh-store attr* type addr* value r*)
      (if (eq? type 'complex-float)
          (qho-store-float attr* addr* value r*)
          (cons (make-qa0-store attr* (convert-type type) addr* value)
                r*)))
    (define (cx->qh-loop attr* var low high code* r*)
      (cons (make-qa0-loop attr* var low high (cx->qh-code* code*))
            r*))
    (define (cx->qh-if var true-code* false-code* r*)
      (cons (make-qa0-if var (cx->qh-code* true-code*)
                         (cx->qh-code* false-code*))
            r*))
    (define (convert-type type)
      (case type
        [(complex-float) 'qh-float]
        [(complex-double) 'qh-double]
        [else type]))

    (lambda (qa0)
      (variant-case qa0
        [qa0-top (decl*)
          (let loop ([decl* decl*] [r* '()])
            (cond
             [(null? decl*) (make-qa0-top (reverse r*))]
             [else (loop (cdr decl*)
                         (cons (cx->qh-decl (car decl*)) r*))]))]))))
