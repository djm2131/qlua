;; Limited formatter
#fload "sfc.sf"
#fload "error.ss"
;;
;; (provide q-fmt*)
;; (provide q-fmt)
;;

;; a copy of the printer...
(define (q-fmt* fmt arg*)
  (define (add-fmt r* a b)
    (if (>= a b) r*
	(string-append r* (substring fmt a b))))
  (define (out-newline r*)
    (string-append r* (string #\newline)))
  (define (out-char c d? r*)
    (cond
     [d? (string-append r* (string c))]
     [(char=? c #\newline) (if d? (out-newline r*)
			       (string-append r* "#\\newline"))]
     [(char=? c #\space) (if d? (string-append r* " ")
			     (string-append r* "#\\space"))]
     [else (string-append (string-append r* "#\\") (string c))]))
  (define (out-string s d? r*)
    (if d? (string-append r* s)
	(let ([l (string-length s)])
	  (let loop ([r* (string-append r* "\"")] [i 0])
	    (cond
	     [(= i l) (string-append r* "\"")]
	     [else
	      (let ([c (string-ref s i)])
		(cond
		 [(or (char=? c #\\) (char=? c #\"))
		  (loop (string-append r* (string #\\ c)) (+ i 1))]
		 [else (loop (string-append r* (string c)) (+ i 1))]))])))))
  (define (out-box x d? r*)
    (fmt/1 (unbox x) d? (out-string "#&" r*)))
  (define (out-vector x d? r*)
    (let ([n (vector-length x)])
      (let loop ([r* (string-append r* "#(")] [i 0])
	(cond
	 [(= i n) (string-append r* ")")]
	 [(zero? i) (loop (fmt/1 (vector-ref x i) d? r*) (+ i 1))]
	 [else (loop (fmt/1 (vector-ref x i) d? (string-append r* " "))
		     (+ i 1))]))))
  (define (out-pair x d? r*)
    (let loop ([r* (string-append r* "(")] [x x])
      (let ([r* (fmt/1 (car x) d? r*)])
	(cond
	 [(pair? (cdr x)) (loop (string-append r* " ") (cdr x))]
	 [(null? (cdr x)) (string-append r* ")")]
	 [else (string-append (fmt/1 (cdr x) d? (string-append r* " . "))
			      ")")]))))
  (define (fmt/1 x d? r*)
    (cond
     [(eof-object? x) (out-string "#<eof>" #t r*)]
     [(input-port? x) (out-string "#<iport>" #t r*)]
     [(output-port? x) (out-string "#<oport>" #t r*)]
     [(symbol? x) (out-string (symbol->string x) #t r*)]
     [(pair? x) (out-pair x d? r*)]
     [(fixnum? x) (out-string (number->string x) #t r*)]
     [(flonum? x) (out-string (number->string x) #t r*)]
     [(null? x) (out-string "()" #t r*)]
     [(boolean? x) (out-string (if x "#t" "#f") #t r*)]
     [(char? x) (out-char x d? r*)]
     [(string? x) (out-string x d? r*)]
     [(vector? x) (out-vector x d? r*)]
     [(box? x) (out-box x d? r*)]
     [(procedure? x) (out-string "#<procedure>" #t r*)]
     [else (out-string "#<unknown>" #t r*)]))
  
  (let ([f-len (string-length fmt)])
    (let loop ([r* ""] [arg* arg*] [a 0] [b 0])
      (cond
       [(= b f-len) (add-fmt r* a b)]
       [(char=? (string-ref fmt b) #\~)
	(if (= b (- f-len 1)) (ic-error 'q-fmt "Dangling ~~, fmt ~s" fmt))
	(let* ([r* (add-fmt r* a b)]
	       [x (+ b 1)]
	       [c (string-ref fmt x)])
	  (if (and (or (char=? c #\a)
		       (char=? c #\s))
		   (null? arg*))
	      (ic-error 'q-fmt "Missing arguments for fmt ~s" fmt))
	  (cond
	   [(char=? c #\a) (loop (fmt/1 (car arg*) #t r*)
				 (cdr arg*) (+ x 1) (+ x 1))]
	   [(char=? c #\s) (loop (fmt/1 (car arg*) #f r*)
				 (cdr arg*) (+ x 1) (+ x 1))]
	   [(char=? c #\~) (loop (string-append r* "~") arg* (+ x 1) (+ x 1))]
	   [(char=? c #\%) (loop (out-newline r*) arg* (+ x 1) (+ x 1))]
	   [else (ic-error 'q-fmt "unrecognized format ~a in ~s" c fmt)]))]
       [else (loop r* arg* a (+ b 1))]))))

(define-syntax q-fmt
  (syntax-rules ()
    [(_ fmt arg ...) (q-fmt* fmt (list arg ...))]))