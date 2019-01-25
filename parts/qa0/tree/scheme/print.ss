;; writer and printer.
#fload "sfc.sf"
#fload "error.ss"
;;
;; (provide q-display)
;; (provide q-fprint)
;; (provide q-print)
;; (provide q-write)
;;
;; (provide q-write/3)
;; (provide q-fprint*)
;;

(define-syntax q-display
  (syntax-rules ()
    [(_ x) (q-write/3 x #t (current-output-port))]
    [(_ x p) (q-write/3 x #t p)]))

(define-syntax q-write
  (syntax-rules ()
    [(_ x) (q-write/3 x #f (current-output-port))]
    [(_ x p) (q-write/3 x #f p)]))

(define-syntax q-fprint
  (syntax-rules ()
    [(_ p f o ...) (q-fprint* p f (list o ...))]))
    
(define-syntax q-print
  (syntax-rules ()
    [(_ f o ...) (q-fprint* (current-output-port) f (list o ...))]))

(define (q-write/3 x d? p)
  (define (wrpair x d? p)
    (write-char #\( p)
    (let loop ([x x])
      (q-write/3 (car x) d? p)
      (cond
       [(pair? (cdr x)) (write-char #\space p) (loop (cdr x))]
       [(null? (cdr x))]
       [else (write-string " . " p) (q-write/3 (cdr x) d? p)]))
    (write-char #\) p))
  
  (define (wrchar x p)
    (cond
     [(char=? x #\newline) (write-string "#\\newline" p)]
     [(char=? x #\space) (write-string "#\\space" p)]
     [else (write-string "#\\" p) (write-char x p)]))

  (define (wrstring x p)
    (write-char #\" p)
    (let ([n (string-length x)])
      (do ([i 0 (+ i 1)])
	  [(= i n)]
        (let ([c (string-ref x i)])
          (if (or (char=? c #\") (char=? c #\\))
              (write-char #\\ p))
          (write-char c p))))
    (write-char #\" p))

  (define (wrvector x d? p)
    (write-string "#(" p)
    (let ([size (vector-length x)])
      (if (not (= size 0))
          (let ([last (- size 1)])
            (let loop ([i 0])
              (q-write/3 (vector-ref x i) d? p)
              (if (not (= i last))
                  (begin (write-char #\space p) (loop (+ i 1))))))))
    (write-char #\) p))

  (define (wrbox x d? p)
    (write-string "#&" p)
    (q-write/3 (unbox x) d? p))

  (cond
   [(eof-object? x) (write-string "#<eof>" p)]
   [(input-port? x) (write-string "#<iport>" p)]
   [(output-port? x) (write-string "#<oport>" p)]
   [(symbol? x) (write-string (symbol->string x) p)]
   [(pair? x) (wrpair x d? p)]
   [(fixnum? x) (display-fixnum x p)]
   [(flonum? x) (display-flonum x p)]
   [(null? x) (write-string "()" p)]
   [(boolean? x) (write-string (if x "#t" "#f") p)]
   [(char? x) (if d? (write-char x p) (wrchar x p))]
   [(string? x) (if d? (write-string x p) (wrstring x p))]
   [(vector? x) (wrvector x d? p)]
   [(box? x) (wrbox x d? p)]
   [(procedure? x) (write-string "#<procedure>" p)]
   [else (write-string "#<unknown>" p)]))

(define (q-fprint* port fstr olst)
  (let loop ([flst (string->list fstr)] [olst olst])
    (cond [(null? flst) #t]
          [(char=? (car flst) #\~)
           (and (pair? (cdr flst))
		(let ([c (cadr flst)])
		  (cond
		   [(char=? c #\a)
		    (unless (null? olst)
		      (q-display (car olst) port)
		      (loop (cddr flst) (cdr olst)))]
		   [(char=? c #\s)
		    (unless (null? olst)
		      (q-write (car olst) port)
		      (loop (cddr flst) (cdr olst)))]
		   [(char=? c #\%) 
		    (newline port) 
		    (loop (cddr flst) olst)]
		   [(char=? c #\~)
		    (write-char #\~ port)
		    (loop (cddr flst) olst)]
		   [else (ic-error* 'format "Unrecognize format char"
				    (cdr flst))])))]
          [else
           (write-char (car flst) port)
           (loop (cdr flst) olst)])))

