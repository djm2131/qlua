;; S-expression parser
#fload "sfc.sf"
#fload "common.sf"
#fload "read.ss"
#fload "error.ss"
#fload "ast.ss"
#fload "cenv.ss"
;;
;; (provide user-reg)
;; (provide parse-qa0-file)
;;
(define (user-reg name) (gen-reg "_" name))

(define (parse-qa0-file file-name)
  ;; make sfc happy
  (define (exactly? a b) (= a b))
  (define (atleast? a b) (>= a b))
  ;; enough of happiness
  (define (dir-name name)
    (let loop ([n (- (string-length name) 1)])
      (cond
       [(negative? n) "./"]
       [(char=? (string-ref name n) #\/) (substring name 0 (+ n 1))]
       [else (loop (- n 1))])))
  (define (make-include-path base include-name)
    (string-append (dir-name base) include-name))
  (define (check-list msg in cmp min-size)
    (if (not (list? in))
        (s-error "List expected in ~a, found~%  ~a~%"
                 msg in))
    (if (not (cmp (length in) min-size))
        (s-error "List of ~a ~a is expected in ~a, found:~%   ~a~%"
                 (if (eq? cmp exactly?) "exactly" "at least")
                 min-size msg in)))
  (define (check-name msg form name)
    (if (not (symbol? name))
        (s-error "Expecting name for ~a, found ~a in:~%~a~%"
                 msg name form)))
  (define (check-sym-or-list-of-syms msg form name)
    (cond
     [(symbol? name) #t]
     [(and (list? name) (let loop ([name name])
                          (cond
                           [(null? name) #t]
                           [(symbol? (car name)) (loop (cdr name))]
                           [else #f]))) #t]
     [else (s-error "Expecting name or list of names for ~a, found ~a in ~a~%"
                    msg name form)]))
  (define (check-string msg form string)
    (if (not (string? string))
        (s-error "Expecting string for ~a, found ~a in:~%~a~%"
                 msg string form)))
  (define (check-attrib* msg form attr*)
    (for-each (lambda (attr) (check-attrib msg form attr)) attr*))
  (define (check-attrib msg form attr)
    (if (not (or (symbol? attr)
                 (and (list? attr)
                      (>= (length attr) 1)
                      (symbol? (car attr)))))
        (s-error "Expecting attribute for ~a, found ~a in~%~a~%"
                 msg attr form)))
  (define (check-output* msg form out*)
    (cond
     [(null? out*) #t]
     [(not (symbol? (car out*)))
      (s-error "Bad value for output of ~a, found ~a in~%~a~%"
               msg (car out*) form)]
     [else (check-output* msg form (cdr out*))]))
  (define (check-input* msg form in*)
    (for-each (lambda (in) (check-input msg form in)) in*))
  (define (check-input msg form in)
    (check-list msg in exactly? 2)
    (if (case (car in)
          [(reg) (not (symbol? (cadr in)))]
          [(const) #f]
          [else #t])
        (s-error "Bad value for input of ~a, found ~a in ~%~a~%"
                 msg in form)))
  (define (check-macro-arg in)
    (check-list "Macro argument" in exactly? 2)
    (if (case (car in)
          [(reg) (not (symbol? (cadr in)))]
          [(const) #f]
          [(macro) #f]
          [else #t])
        (s-error "Bad value for macro argument ~a" in)))
  (define (check-iterator msg form iter)
    (check-list msg iter atleast? 2)
    (check-name msg form (car iter))
    (case (length iter)
      [(2) (check-list "iterator range" (cadr iter) atleast? 0)]
      [(3) #t]
      [else (s-error "Bad iterator form of ~a, found ~a in~%~a~%"
                   msg iter form)]))
  (define (empty-ast) (make-qa0-top '()))
  (define (parse-top s-expr ast)
    (check-list "top level" s-expr atleast? 1)
    (variant-case ast
      [qa0-top (decl*)
        (case (car s-expr)
          [(alias)
           (make-qa0-top (append decl* (list (parse-alias s-expr))))]
          [(constant)
           (make-qa0-top (append decl* (list (parse-constant s-expr))))]
          [(array)
           (make-qa0-top (append decl* (list (parse-array s-expr))))]
          [(structure)
           (make-qa0-top (append decl* (list (parse-structure s-expr))))]
          [(repeat procedure)
           (make-qa0-top (append decl* (parse-repeat s-expr)))]
          [(include)
           (make-qa0-top (append decl* (parse-include s-expr)))]
          [(define)
           (make-qa0-top (append decl* (list (parse-define s-expr))))]
          [(verbose)
           (make-qa0-top (append decl* (list (parse-verbose s-expr))))]
          [(name-prefix)
           (make-qa0-top (append decl* (list (parse-name-prefix s-expr))))]
          [(name-infix)
           (make-qa0-top (append decl* (list (parse-name-infix s-expr))))]
          [(name-suffix)
           (make-qa0-top (append decl* (list (parse-name-suffix s-expr))))]
          [else (s-error "Unknown form:~%~a~%" s-expr)])]))
  (define (parse-include s-expr)
    (check-list "include form" s-expr exactly? 2)
    (check-string "include file name" s-expr (cadr s-expr))
    (let ([f (open-input-file (make-include-path file-name (cadr s-expr)))])
      (let loop ([r (q-read f)] [ast (empty-ast)])
        (cond
         [(eof-object? r) (close-input-port f) (qa0-top->decl* ast)]
         [else (loop (q-read f) (parse-top r ast))]))))
  (define (parse-name-infix s-expr)
    (check-list "name-infix form" s-expr exactly? 2)
    (check-string "name-infix name" s-expr (cadr s-expr))
    (make-qa0-name-infix (cadr s-expr)))
  (define (parse-name-prefix s-expr)
    (check-list "name-prefix form" s-expr exactly? 2)
    (check-string "name-prefix name" s-expr (cadr s-expr))
    (make-qa0-name-prefix (cadr s-expr)))
  (define (parse-name-suffix s-expr)
    (check-list "name-suffix form" s-expr exactly? 2)
    (check-string "name-suffix name" s-expr (cadr s-expr))
    (make-qa0-name-suffix (cadr s-expr)))
  (define (parse-verbose s-expr)
    (check-list "verbose form" s-expr atleast? 1)
    (map (lambda (x)
           (check-list "verbose case" x exactly? 2)
           (check-sym-or-list-of-syms "verbose target" s-expr (car x))
           (check-string "verbose data" s-expr (cadr x)))
         (cdr s-expr))
    (make-qa0-verbose (map car (cdr s-expr)) (map cadr (cdr s-expr))))
  (define (parse-alias s-expr)
    (check-list "top level alias" s-expr exactly? 3)
    (check-name "alias new name" s-expr (cadr s-expr))
    (check-name "alias old name" s-expr (caddr s-expr))
    (make-qa0-alias (caddr s-expr) (cadr s-expr)))
  (define (parse-constant s-expr)
    (check-list "constant" s-expr exactly? 3)
    (check-name "constant name" s-expr (cadr s-expr))
    (make-qa0-const (cadr s-expr) (parse-const-expr (caddr s-expr))))
  (define (parse-array s-expr)
    (check-list "array" s-expr exactly? 5)
    (check-name "array name" s-expr (cadr s-expr))
    (check-string "array c-name" s-expr (caddr s-expr))
    (check-name "array base name" s-expr (cadddr s-expr))
    (make-qa0-array (cadr s-expr) (caddr s-expr) (cadddr s-expr)
                    (parse-const-expr (car (cddddr s-expr)))))
  (define (parse-structure s-expr)
    (check-list "structure" s-expr exactly? 4)
    (let ([name (cadr s-expr)]
          [c-name (caddr s-expr)]
          [f* (cadddr s-expr)])
      (check-name "struct name" s-expr name)
      (check-string "struct c-name" s-expr c-name)
      (map (lambda (f)
             (check-list "struct field" f exactly? 3)
             (check-name "struct field name" s-expr (car f))
             (check-string "struct field c-name" s-expr (cadr f))
             (check-name "struct field type" s-expr (caddr f)))
           f*)
      (make-qa0-struct name c-name
                       (map car f*) (map caddr f*) (map cadr f*))))
  (define (parse-repeat s-expr)
    (check-list "top level repeat" s-expr atleast? 1)
    (case (car s-expr)
      [(repeat)
       (check-list "top level repeat" s-expr atleast? 3)
       (let loop ([bind* (cadr s-expr)])
         (cond
          [(null? bind*)
           (let do-body ([p* '()] [b* (cddr s-expr)])
             (cond
              [(null? b*) p*]
              [else (do-body (append p* (parse-repeat (car b*)))
                             (cdr b*))]))]
          [else
           (check-iterator "top level repeat" s-expr (car bind*))
           (list
            (case (length (car bind*))
              [(2) (make-qa0-repeat-list (caar bind*)
                                         (map parse-a-value (cadar bind*))
                                         (loop (cdr bind*)))]
              [(3) (make-qa0-repeat-range (caar bind*)
                                          (parse-const-expr (cadar bind*))
                                          (parse-const-expr (caddar bind*))
                                          (loop (cdr bind*)))]
              [else (s-error "Bad binding in parse-repeat: ~a"
                             (car bind*))]))]))]
      [(procedure) (list (parse-procedure s-expr))]
      [else (s-error "Bad form at top level~%~a~%" s-expr)]))
  (define (parse-a-value expr)
    (if (or (number? expr)
            (symbol? expr)
            (string? expr)) expr
            (s-error "a-value is not ~a~%" expr)))
  (define (parse-procedure s-expr)
    (check-list "procedure" s-expr atleast? 5)
    (check-name "procedure name" s-expr (cadr s-expr))
    (check-attrib* "procedure attributes" s-expr (caddr s-expr))
    (check-list "procedure arguments" (cadddr s-expr) atleast? 0)
    (map (lambda (arg)
           (check-list "procedure argument" arg exactly? 4)
           (check-name "argument name" arg (car arg))
           (check-name "argument type" arg (cadr arg))
           (check-string "argument c-name" arg (caddr arg))
           (check-string "argument c-type" arg (cadddr arg)))
         (cadddr s-expr))
    (parse-code* (cddddr s-expr)
                 (lambda (code*)
                   (make-qa0-proc (map parse-attr (caddr s-expr))
                                  (cadr s-expr)
                                  (map user-reg (map car (cadddr s-expr)))
                                  (map cadr (cadddr s-expr))
                                  (map caddr (cadddr s-expr))
                                  (map cadddr (cadddr s-expr))
                                  code*))))
  (define (parse-define s-expr)
    (check-list "define" s-expr atleast? 3)
    (check-list "define spec" (cadr s-expr) atleast? 1)
    (check-name "define name" s-expr (caadr s-expr))
    (map (lambda (arg) (check-name "parameter name" s-expr arg))
         (cdadr s-expr))
    (parse-code* (cddr s-expr)
                 (lambda (code*)
                   (make-qa0-macro-def (caadr s-expr)
                                       (cdadr s-expr)
                                       code*))))
  (define (parse-code* code* k)
    (if (null? code*) (k '())
        (parse-code* (cdr code*)
                     (lambda (op*)
                       (let ([code (car code*)])
                         (check-list "operation" code atleast? 1)
                         (case (car code)
                           [(comment) (k op*)]
                           [(begin)
                            (parse-code* (cdr code)
                                         (lambda (op2*)
                                           (k (append op2* op*))))]
                           [(macro) (k (cons (parse-macro code) op*))]
                           [(repeat) (k (parse-foreach code op*))]
                           [else (k (cons (parse-code code) op*))]))))))
  (define (parse-macro f)
    (check-list "macro" f atleast? 2)
    (check-name "macro name" f (cadr f))
    (make-qa0-macro-call (cadr f) (map parse-macro-arg (cddr f))))
  (define (parse-attr f)
    (cond
     [(symbol? f) (make-qa0-attr f '())]
     [else (check-list "attribute" f atleast? 1)
           (make-qa0-attr (car f) (map parse-a-value (cdr f)))]))
  (define (parse-code f)
    (check-list "code" f atleast? 1)
    (case (car f)
      [(op) (parse-op f)]
      [(load) (parse-load f)]
      [(store) (parse-store f)]
      [(loop) (parse-loop f)]
      [(if) (parse-if f)]
      [(if-else) (parse-if-else f)]
      [(macro) (parse-macro f)]
      [else (s-error "Unexpected code~%~a~%" f)]))
  (define (parse-op f)
    (check-list "code op" f exactly? 5)
    (let ([name (cadr f)]
          [attr* (caddr f)]
          [out* (cadddr f)]
          [in* (car (cddddr f))])
      (check-name "code op name" f name)
      (check-attrib* "code op attirbutes" f attr*)
      (check-output* "code op outputs" f out*)
      (check-input* "code op inputs" f in*)
      (make-qa0-operation (map parse-attr attr*)
                          name
                          (map parse-output out*)
                          (map parse-input in*))))
  (define (parse-output out) (parse-reg out))
  (define (parse-macro-arg arg)
    (check-macro-arg arg)
    (case (car arg)
      [(reg) (parse-reg (cadr arg))]
      [(const) (parse-const-expr arg)]
      [(macro) (make-c-expr-macro (cadr arg))]
      [else (s-error "Unexpected macro argument ~a" arg)]))
  (define (parse-input in)
    (case (car in)
      [(reg) (parse-reg (cadr in))]
      [(const) (parse-const-expr in)]
      [else (s-error "Unexpected form of input ~a" in)]))
  (define (parse-reg name) (make-reg (user-reg name)))
  (define (parse-load f)
    (check-list "load" f exactly? 5)
    (let ([type (cadr f)]
          [attr* (caddr f)]
          [out (cadddr f)]
          [addr (car (cddddr f))])
      (check-attrib* "load" f attr*)
      (check-name "load" f type)
      (check-output* "load" f (list out))
      (check-input* "load" f addr)
      (make-qa0-load (map parse-attr attr*)
                     type
                     (parse-output out)
                     (map parse-input addr))))
  (define (parse-store f)
    (check-list "store" f exactly? 5)
    (let ([type (cadr f)]
          [attr* (caddr f)]
          [addr (cadddr f)]
          [value (car (cddddr f))])
      (check-attrib* "store" f attr*)
      (check-name "store" f type)
      (check-input* "store" f addr)
      (check-input "store" f value)
      (make-qa0-store (map parse-attr attr*)
                      type
                      (map parse-input addr)
                      (parse-input value))))
  (define (parse-loop f)
    (check-list "loop" f atleast? 4)
    (let ([attr* (cadr f)]
          [ctl (caddr f)]
          [code* (cdddr f)])
      (check-attrib* "loop" f attr*)
      (check-list "loop control" ctl exactly? 3)
      (parse-code* code* (lambda (op*)
                           (make-qa0-loop (map parse-attr attr*)
                                          (parse-output (car ctl))
                                          (parse-input (cadr ctl))
                                          (parse-input (caddr ctl))
                                          op*)))))
  (define (parse-if f)
    (check-list "if" f exactly? 3)
    (let ([c (cadr f)]
          [t (caddr f)])
      (check-input "if predicate" f c)
      (check-list "if branch" t atleast? 1)
      (make-qa0-if (parse-input c) (parse-seq t) '())))
  (define (parse-if-else form)
    (check-list "if-else" form exactly? 4)
    (let ([c (cadr form)]
          [t (caddr form)]
          [f (cadddr form)])
      (check-input "if-else predicate" form c)
      (check-list "if-else true branch" t atleast? 1)
      (check-list "if-else false branch" f atleast? 1)
      (make-qa0-if (parse-input c) (parse-seq t) (parse-seq f))))
  (define (parse-seq s)
    (if (and (list s) (> (length s) 0) (eq? (car s) 'begin))
        (parse-code* (cdr s) (lambda (p*) p*))
        (list (parse-code s))))
  (define (parse-foreach form op*)
    (check-list "macro" form atleast? 3)
    (let ([iterator* (cadr form)]
          [code* (cddr form)])
      (let loop ([i* iterator*] [t* op*])
        (cond
         [(null? i*) (parse-code* code* (lambda (c*) (append c* t*)))]
         [else (let ([i (car i*)] [i* (cdr i*)])
                 (check-iterator "macro" form i)
                 (case (length i)
                   [(2) (let ([c* (loop i* '())]
                              [v* (map parse-a-value (cadr i))]
                              [id (car i)])
                          (cons (make-qa0-macro-list id v* c*) t*))]
                   [(3) (let ([c* (loop i* '())]
                              [id (car i)]
                              [lo (parse-const-expr (cadr i))]
                              [hi (parse-const-expr (caddr i))])
                          (cons (make-qa0-macro-range id lo hi c*) t*))]
                   [else (s-error "Internal error in parse-foreach")]))]))))
  (define (parse-const-expr form)
    (check-list "constant expression" form exactly? 2)
    (parse-c-expr (cadr form)))
  (define (parse-c-expr form)
    (cond
     [(number? form) (make-c-expr-number form)]
     [(symbol? form) (make-c-expr-id form)]
     [(string? form) (make-c-expr-string form)]
     [(and (= (length form) 2) (eq? (car form) 'quote))
      (make-c-expr-quote (cadr form))]
     [(and (list? form) (>= (length form) 1) (symbol? (car form)))
      (make-c-expr-op (car form) (map parse-c-expr (cdr form)))]
     [else (s-error "constant expression is bad:~%~a~%" form)]))
  (let ([f (open-input-file file-name)])
    (let loop ([s-expr (q-read f)] [ast (empty-ast)])
      (cond
       [(eof-object? s-expr) ast]
       [else (loop (q-read f) (parse-top s-expr ast))]))))
