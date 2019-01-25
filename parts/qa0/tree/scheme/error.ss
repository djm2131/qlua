;; Error handling

#fload "sfc.sf"
#fload "print.ss"

;; Capture the output port
(define error-port (current-output-port))

;; Reader errors
(define (r-error* msg arg*)
  (newline error-port)
  (q-display "QA0: Reader error: " error-port)
  (q-display msg error-port)
  (for-each (lambda (arg) 
	      (write-char #\space)
	      (if (string? arg) (q-display arg error-port) (q-write arg))) 
	    arg*)
  (newline error-port)
  (flush-output-port error-port)
  (reset))

(define-syntax r-error
  (syntax-rules ()
    [(_ msg arg ...) (r-error* msg (list arg ...))]))

;; Syntax errors
(define (s-error* msg arg*)
  (newline error-port)
  (q-display "Syntax error: " error-port)
  (q-fprint* error-port msg arg*)
  (newline error-port)
  (flush-output-port error-port)
  (reset))

(define-syntax s-error
  (syntax-rules ()
    [(_ msg arg ...) (s-error* msg (list arg ...))]))

;; Internal compiler errors, aka ICE
(define (ic-error* loc msg arg*)
  (newline error-port)
  (q-display "QA0 ICE: " error-port)
  (q-display loc error-port)
  (q-display ": " error-port)
  (q-fprint* error-port msg arg*)
  (newline error-port)
  (flush-output-port error-port)
  (reset))

(define-syntax ic-error
  (syntax-rules ()
    [(_ loc msg arg ...) (ic-error* loc msg (list arg ...))]))