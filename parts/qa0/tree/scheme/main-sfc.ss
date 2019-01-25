;; Main for #F
#fload "sfc.sf"
#fload "common.sf"
#fload "qa0.ss"

(define (main argv)
  (let ([arg* (list->vector (cdr (argv->list argv)))])
    (qa0-driver arg*)
    (exit 0)))
