;; emit routine for verbose AST
#fload "sfc.sf"
#fload "print.ss"
;;
;; (provide emit-verbose)
;;
(define (emit-verbose key target* data*)
  (define (target-match? target)
    (define (do-match k)
      (or (eq? k target)
          (and (list? target)
               (memq k target))))
    (cond
     [(memq key '(cee cee-32)) (or (do-match 'cee) (do-match 'cee-32))]
     [(memq key '(c99 c99-32)) (or (do-match 'c99) (do-match 'c99-32))]
     [else (do-match key)]))
  (let loop ([target* target*] [data* data*])
    (cond
     [(null? target*) (q-print "~%")]
     [(target-match? (car target*))
      (q-print "~a" (car data*))
      (loop (cdr target*) (cdr data*))]
     [else (loop (cdr target*) (cdr data*))])))

