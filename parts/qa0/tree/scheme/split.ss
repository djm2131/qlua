;; Spliting operations
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "ast.ss"

(define (/-check-io cl name r* len)
      (if (not (= (length r*) len))
          (s-error "op ~a: ~a should be of ~a elements, found ~a"
                   name cl len r*)))

(define (/in-all x in*) (cons x in*))

(define (/in-tail x in*) (cons x (cdr in*)))

(define /-fresh-reg
  (let ([*reg-count* 0])
    (lambda ()
      (let ([x (gen-reg 'b *reg-count*)])
        (set! *reg-count* (+ *reg-count* 1))
        (make-reg x)))))

;; convert (op old out* in*) into
;;    (op new out* in*)
(define (/-rename out-count in-count new-name)
  (lambda (old-name attr* output* input* r*)
    (/-check-io "output" old-name output* out-count)
    (/-check-io "inaput" old-name input* in-count)
    (cons (make-qa0-operation attr* new-name output* input*) r*)))

;; convert (op old out* in*) into
;;   (op new-a (f) in*)
;;   (op new-b out* (b-filter f in*))
(define (/-split2 out-count in-count new-name-a b-filter new-name-b)
  (lambda (old-name attr* output* input* r*)
    (/-check-io "output" old-name output* out-count)
    (/-check-io "inaput" old-name input* in-count)
    (let ([x (/-fresh-reg)])
      (list* (make-qa0-operation attr* new-name-b output* (b-filter x input*))
             (make-qa0-operation attr* new-name-a (list x) input*)
             r*))))
