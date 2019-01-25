; S-expression reader.

; Adapted from #F SFC reader
; Adapted from R6RS reference implementation's Scheme reader.

; Copyright (c) 1993-2006 by Richard Kelsey, Jonathan Rees, and Mike Sperber
; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#fload "sfc.sf"
#fload "error.ss"

;------------------------------------------------------------------------------

; reader

(define q-read/1 ;; port --> s-expr
  (let ()
    (define *non-symbol-constituents-above-127* 
      '#(128 129 130 131 132 133 134 135 136 137 138 139 140 
	     141 142 143 144 145 146 147 148 149 150 151 152 153 
	     154 155 156 157 158 159 160 171 173 187)) ; 8-bit only
    
    (define *whitespaces* '#(9 10 11 12 13 32 160)) ; 8-bit only

    (define (read/1 port) ;aka get-datum
      (let loop ()
	(let ([form (sub-read port)])
	  (cond
	   [(not (reader-token? form)) form]
	   [(eq? form close-paren)
	    (r-error "extraneous right parenthesis" port)]
	   [(eq? form close-bracket)
	    (r-error "discarding extraneous right bracket" port)]
	   [else (r-error "unexpected datum" (cdr form) port)]))))

    (define (sub-read-carefully port)
      (let ([form (sub-read port)])
	(cond
	 [(eof-object? form)
	  (r-error "unexpected end of file" port)]
	 [(reader-token? form) (r-error "unexpected token" (cdr form) port)]
	 [else form])))

    (define reader-token-marker (list 'reader-token))

    (define (make-reader-token message)
      (cons reader-token-marker message))

    (define (reader-token? form)
      (and (pair? form) (eq? (car form) reader-token-marker)))

    (define close-paren
      (make-reader-token "unexpected right parenthesis"))

    (define close-bracket
      (make-reader-token "unexpected right bracket"))

    (define dot (make-reader-token "unexpected \" . \""))

    ;; main dispatch

    (define *dispatch-table-limit* 128)

    (define read-dispatch-vector
      (make-vector
       *dispatch-table-limit*
       (lambda (c port)
	 (r-error "illegal character read" c port))))

    (define read-terminating?-vector
      (make-vector *dispatch-table-limit* #t))

    (define (set-standard-syntax! char terminating? reader)
      (vector-set! read-dispatch-vector (char->integer char) reader)
      (vector-set! read-terminating?-vector (char->integer char) terminating?))

    (define (sub-read port)
      (let ([c (read-char port)])
	(if (eof-object? c) c
	    (let ([scalar-value (char->integer c)])
	      (cond
	       [(< scalar-value *dispatch-table-limit*)
		((vector-ref read-dispatch-vector (char->integer c)) c port)]
	       [(char-alphabetic? c) (sub-read-constituent c port)]
	       [else (r-error "illegal character read" c port)])))))

    (define (sub-read-constituent c port)
      (parse-token (sub-read-token c port) port))

    (define (set-standard-read-macro! c terminating? proc)
      (set-standard-syntax! c terminating? proc))

    (define (sub-read-list c port close-token)
      (let ([form (sub-read port)])
	(if (eq? form dot)
	    (r-error "missing car -- ( immediately followed by ." port)
	    (let recur ([form form])
	      (cond
	       [(eof-object? form)
		(r-error "eof inside list -- unbalanced parentheses" port)]
	       [(eq? form close-token) '()]
	       [(eq? form dot)
		(let*
		    ([last-form (sub-read-carefully port)]
		     [another-form (sub-read port)])
		  (if (eq? another-form close-token) last-form
		      (r-error "randomness after form after dot"
			       another-form port)))]
	       [else (cons form (recur (sub-read port)))])))))

    (define (sub-read-list-paren c port)
      (sub-read-list c port close-paren))

    (define (sub-read-list-bracket c port)
      (sub-read-list c port close-bracket))
    
    (define *nul* (integer->char 0))
    (define *alarm* (integer->char 7))
    (define *backspace* (integer->char 8))
    (define *tab* (integer->char 9))
    (define *linefeed* (integer->char 10))
    (define *vtab* (integer->char 11))
    (define *page* (integer->char 12))
    (define *return* (integer->char 13))
    (define *escape* (integer->char 27))
    (define *delete* (integer->char 127))
    
    (define (decode-escape port)
      (let ([c (read-char port)])
	(if (eof-object? c)
	    (r-error "end of file within a string" port))
	(let ([scalar-value (char->integer c)])
	  (cond
	   [(or (char=? c #\\) (char=? c #\")) c]
	   [(char=? c *linefeed*)
	    (let loop ()
	      (let ([c (peek-char port)])
		(cond
		 [(eof-object? c)
		  (r-error "end of file within a string" port)]
		 [(char-unicode-whitespace? c)
		  (read-char port) (loop)]
		 [else #f])))]
	   [(char=? c #\a) *alarm*]
	   [(char=? c #\b) *backspace*]
	   [(char=? c #\t) *tab*]
	   [(char=? c #\n) *linefeed*]
	   [(char=? c #\v) *vtab*]
	   [(char=? c #\f) *page*]
	   [(char=? c #\r) *return*]
	   [(char=? c #\e) *escape*]
	   [(char=? c #\x)
	    (let
		([d (decode-hex-digits port char-semicolon? "string literal")])
	      (read-char port) d)]
	   [else (r-error "invalid escaped character in string" c port)]))))

    (define (char-semicolon? c) (and (char? c) (char=? c #\;)))

    (define (decode-hex-digits port delimiter? desc)
      (let loop ([rev-digits '()])
	(let ([c (peek-char port)])
	  (cond
	   [(delimiter? c)
	    (integer->char
	     (string->number (list->string (reverse rev-digits)) 16))]
	   [(eof-object? c)
	    (r-error
	     (string-append
	      "premature end of a scalar-value literal within a "
	      desc)
	     port)]
	   [(not (char-hex-digit? c))
	    (r-error
	     (string-append "invalid hex digit in a " desc) c port)]
	   [else (read-char port) (loop (cons c rev-digits))]))))
    
    (define (char-hex-digit? c)
      (let ([scalar-value (char->integer c)])
	(or (and (>= scalar-value 48) (<= scalar-value 57))
	    (and (>= scalar-value 65) (<= scalar-value 70))
	    (and (>= scalar-value 97) (<= scalar-value 102)))))

    (define (gobble-line port)
      (let loop ()
	(let ([c (read-char port)])
	  (cond
	   [(eof-object? c) c]
	   [(char=? c *linefeed*) #f]
	   [else (loop)]))))

    (define *sharp-macros* '())

    (define (set-sharp-macro! c proc)
      (set! *sharp-macros* (cons (cons c proc) *sharp-macros*)))

    (define *char-name-table*
      (list
       (cons 'space #\space)
       (cons 'nul *nul*)
       (cons 'alarm *alarm*)
       (cons 'backspace *backspace*)
       (cons 'tab *tab*)
       (cons 'newline *linefeed*) ; not in r6rs draft????
       (cons 'linefeed *linefeed*)
       (cons 'vtab *vtab*)
       (cons 'page *page*)
       (cons 'esc *escape*)
       (cons 'delete *delete*)))

    (define (char-scalar-value-literal-delimiter? c)
      (or (eof-object? c) (delimiter? c)))

    (define (proper-list? x)
      (cond
       [(null? x) #t] 
       [(pair? x) (proper-list? (cdr x))] 
       [else #f]))

    (define (sub-read-token c port)
      (let loop ([l (list c)] [n 1])
	(let ([c (peek-char port)])
	  (cond
	   [(eof-object? c)
	    (reverse-list->string l n)]
	   [(char=? c #\\)
	    (read-char port)
	    (let ([c (peek-char port)])
	      (cond
	       [(or (eof-object? c) (not (char=? #\x c)))
		(r-error
		 "invalid escape sequence in a symbol" c port)]
	       [else
		(read-char port)
		(let ([d (decode-hex-digits port char-semicolon?
					    "symbol literal")])
		  (read-char port) 
		  (loop (cons d l) (+ n 1)))]))]
	   [else
	    (let ([sv (char->integer c)])
	      (if (if (< sv *dispatch-table-limit*)
		      (vector-ref read-terminating?-vector sv)
		      (binary-search
		       *non-symbol-constituents-above-127* sv))
		  (reverse-list->string l n)
		  (begin (read-char port)
			 (loop (cons c l) (+ n 1)))))]))))

    (define (parse-token string port)
      (if (let ([c (string-ref string 0)])
	    (or (char-numeric? c)
		(char=? c #\+)
		(char=? c #\-)
		(char=? c #\.)))
	  (cond [(string->number string)]
		[(member string strange-symbol-names)
		 (string->symbol (make-immutable! string))]
		[(string=? string ".") dot]
		[(and (>= (string-length string) 2)
		      (char=? (string-ref string 0) #\-)
		      (char=? (string-ref string 1) #\>))
		 (string->symbol (make-immutable! string))]
		[else (r-error "unsupported number syntax" string port)])
	  (string->symbol (make-immutable! string))))

    (define strange-symbol-names '("+" "-" "..."))

    (define (delimiter? c)
      (or (char-unicode-whitespace? c)
	  (char=? c #\))
	  (char=? c #\()
	  (char=? c #\])
	  (char=? c #\[)
	  (char=? c #\")
	  (char=? c #\;)))

    (define (char-unicode-whitespace? c)
      (binary-search *whitespaces* (char->integer c)))

    (define (binary-search vec val)
      (let ([size (vector-length vec)])
	(let loop ([low 0] [high size])
	  (cond
	   [(< low (- high 1)) (let* ([pos (quotient (+ low high) 2)]
				      [at (vector-ref vec pos)])
				 (cond [(= val at) pos]
				       [(< val at) (loop low pos)]
				       [else (loop pos high)]))]
	   [(< low high) (if (= val (vector-ref vec low)) low #f)]
	   [else #f]))))

    (define (reverse-list->string l n) 
      (list->string (reverse l)))
    
    (define (make-immutable! x) x)

    ;; settings
    (let ([sub-read-whitespace 
	   (lambda (c port) ; c is ignored 
	     (sub-read port))])
      (for-each (lambda (c)
		  (vector-set! read-dispatch-vector c sub-read-whitespace))
		'(32 9 10 11 12 13)))

    (for-each
     (lambda (c) (set-standard-syntax! c #f sub-read-constituent))
     (string->list
      (string-append
       "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLM"
       "NOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

    (set-standard-read-macro! #\( #t sub-read-list-paren)
    (set-standard-read-macro! #\[ #t sub-read-list-bracket)
    (set-standard-read-macro! #\) #t (lambda (c port) c port close-paren))
    (set-standard-read-macro! #\] #t (lambda (c port) c port close-bracket))
    (set-standard-read-macro! #\' #t
			      (lambda (c port)
				c (list 'quote (sub-read-carefully port))))

    (set-standard-read-macro! #\` #t
			      (lambda (c port)
				c (list 'quasiquote (sub-read-carefully port))))

    (set-standard-read-macro! #\, #t
			      (lambda (c port)
				c
				(let*
				    ([next (peek-char port)]
				     [keyword
				      (cond
				       [(eof-object? next)
					(r-error "end of file after ," port)]
				       [(char=? next #\@) (read-char port)
					'unquote-splicing]
				       [else 'unquote])])
				  (list keyword (sub-read-carefully port)))))

    (set-standard-read-macro! #\" #t
			      (lambda (c port)
				c
				(let loop ([l '()] [i 0])
				  (let ([c (read-char port)])
				    (cond
				     [(eof-object? c)
				      (r-error "end of file within a string"
					       port)]
				     [(char=? c #\\)
				      (cond
				       [(decode-escape port)
					=> (lambda (e)
					     (loop (cons e l) (+ i 1)))]
				       [else (loop l i)])]
				     [(char=? c #\")
				      (reverse-list->string l i)]
				     [else (loop (cons c l) (+ i 1))])))))

    (set-standard-read-macro! #\; #t
			      (lambda (c port)
				c (gobble-line port) (sub-read port)))

    (set-standard-read-macro!  #\# #f
			       (lambda (c port)
				 c
				 (let*
				     ([c (peek-char port)]
				      [c (if (eof-object? c)
					     (r-error "end of file after #"
						      port)
					     (char-downcase c))]
				      [probe (assq c *sharp-macros*)])
				   (if probe ((cdr probe) c port)
				       (r-error "unknown # syntax" c port)))))
    
    (set-sharp-macro! #\f (lambda (c port) (read-char port) #f))
    (set-sharp-macro! #\t (lambda (c port) (read-char port) #t))
    (set-sharp-macro! #\' (lambda (c port)
			    (read-char port) 
			    (list 'syntax (sub-read-carefully port))))
    (set-sharp-macro! #\` (lambda (c port)
			    (read-char port) 
			    (list 'quasisyntax (sub-read-carefully port))))
    (set-sharp-macro! #\, (lambda (c port)
			    (read-char port)
			    (let*
				([next (peek-char port)]
				 [keyword
				  (cond
				   [(eof-object? next)
				    (r-error "end of file after ," port)]
				   [(char=? next #\@) (read-char port)
				    'unsyntax-splicing]
				   [else 'unsyntax])])
			      (list keyword (sub-read-carefully port)))))
    ;; esl++ box reader
    (set-sharp-macro! #\& (lambda (c port)
			    (read-char port)
			    (let ([next (peek-char port)])
			      (if (eof-object? next)
				  (r-error "end of file after &" port)
				  (box (sub-read-carefully port))))))
    ;; esl* - bug fixes
    (set-sharp-macro! #\| (lambda (c port)
			    (read-char port)
			    (let recur () ;starts right after opening #|
			      (let ([next (read-char port)])
				(cond
				 [(eof-object? next)
				  (r-error "end of file in #| comment" port)]
				 [(char=? next #\|)
				  (let ([next (peek-char port)])
				    (cond
				     [(eof-object? next)
				      (r-error "end of file in #| comment"
					       port)]
				     [(char=? next #\#) (read-char port)]
				     [else (recur)]))]
				 [(char=? next #\#)
				  (let ([next (peek-char port)])
				    (cond
				     [(eof-object? next)
				      (r-error "end of file in #| comment"
					       port)]
				     [(char=? next #\|)
				      (read-char port) (recur) (recur)]
				     [else (recur)]))]
				 [else (recur)])))
			    (sub-read port)))

    (set-sharp-macro! #\; (lambda (char port)
			    (read-char port)
			    (sub-read-carefully port)
			    (sub-read port)))

    (set-sharp-macro! #\\ (lambda (c port)
			    (read-char port)
			    (let ([c (peek-char port)])
			      (cond
			       [(eof-object? c)
				(r-error "end of file after #\\" port)]
			       [(char=? #\x c)
				(read-char port)
				(if (delimiter? (peek-char port)) c
				    (decode-hex-digits
				     port
				     char-scalar-value-literal-delimiter?
				     "char literal"))]
			       [(char-alphabetic? c)
				(let ([name (sub-read-carefully port)])
				  (cond
				   [(= (string-length (symbol->string name)) 1)
				    c]
				   [(assq name *char-name-table*) => cdr]
				   [else (r-error "unknown #\\ name"
						  name port)]))]
			       [else (read-char port) c]))))

    (set-sharp-macro! #\( (lambda (c port)
			    (read-char port)
			    (let ([elts (sub-read-list-paren c port)])
			      (if (proper-list? elts)
				  (list->vector elts)
				  (r-error "dot in #(...)" port)))))
    ;; esl++
    (for-each
     (lambda (char)
       (set-sharp-macro! char (lambda (c port)
				(let loop ([n 0] [c (read-char port)])
				  (if (char-numeric? c)
				      (loop (+ (* n 10)
					       (- (char->integer c)
						  (char->integer #\0)))
					    (read-char port))
				      (if (char=? c #\')
					  (list 'function
						(do ([i n (- i 1)]
						     [l '() (cons '? l)])
						    [(zero? i) l])
						(sub-read-carefully port))
					  (r-error "unknown #\\<digits> macro"
						   port)))))))
     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

    (let
	([number-sharp-macro (lambda (c port)
			       (let ([string (sub-read-token #\# port)])
				 (or (string->number string)
				     (r-error "unsupported number syntax"
					      string port))))])
      (for-each (lambda (c) (set-sharp-macro! c number-sharp-macro))
		'(#\b #\o #\d #\x #\i #\e)))

    ;; the reader procedure
    read/1))

(define-syntax q-read
  (syntax-rules ()
    [(_) (q-read/1 (current-input-port))]
    [(_ p) (q-read/1 p)]))

