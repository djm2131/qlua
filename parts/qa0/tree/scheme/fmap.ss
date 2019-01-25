;; Functional map.
;;
#fload "sfc.sf"
#fload "common.sf"
#fload "format.ss"
#fload "error.ss"

;; key->cpo
(define (fmap-build-key k) (q-fmt "~s" k))
;; CPO
(define (fmap-compare a b) ; => -1,0,1; transitive and antisymmetric
  (cond
   [(string<? a b) +1]
   [(string>? a b) -1]
   [else 0]))

;; treap variants
(define-variant treap-empty ())
(define-variant treap-node (key verbatim value hash left right))

;;;;;
;(define (dump-fmap fm)
;  (fprintf error-port "~%FMAP DUMP~%")
;  (let loop ([fm fm] [p ""] [r ">"])
;    (variant-case fm
;      [treap-empty () #f]
;      [treap-node (key verbatim value left right)
;	(loop left (string-append p " ") " ")
;	(fprintf error-port "~a~a~s ...~%" r p key)
;	(loop right (string-append p " ") " ")]))
;  (fprintf error-port "FMAP END~%~%"))
;
;; fmap operations
(define empty-fmap
  (let ([e (make-treap-empty)])
    (lambda () e)))

(define (lookup-fmap fm k success failure)
  (let ([mk (fmap-build-key k)])
    (let loop ([fm fm])
      (variant-case fm
	[treap-empty () (failure)]
	[treap-node (key value left right)
  	  (let ([z (fmap-compare mk key)])
	    (case z
	      [(-1) (loop left)]
	      [(+1) (loop right)]
	      [else (success value)]))]))))
  
(define extend-fmap
  (let ()
    (define fmap-random!
      (let ([rstep 0.6180339887498948] [x 0.0])
	(lambda ()
	  (let ([y x]
		[z (fl+ x rstep)])
	    (set! x (if (fl<? z 1.0) z (fl- z 1.0)))
	    y))))
    (define (ref-ptr p) ((car p)))
    (define (set-ptr! p v) ((cdr p) v))
    (define (mk-left p)
      (cons (lambda () (treap-node->left p))
	    (lambda (v) (set-treap-node-left! p v))))
    (define (mk-right p)
      (cons (lambda () (treap-node->right p))
	    (lambda (v) (set-treap-node-right! p v))))
    (lambda (fm k v)
      (define mk (fmap-build-key k))
      (define (mk-addr) (cons (lambda () fm) (lambda (v) (set! fm v))))
      (define (update ignore)
	(let loop ([p (mk-addr)])
	  (let ([ref (ref-ptr p)])
	    (variant-case ref
	      [treap-node (key hash left right)
		(let ([z (fmap-compare mk key)])
		  (case z
		    [(0) (set-ptr! p (make-treap-node mk k v hash left right))
		         fm]
		    [(-1) (let ([x (copy ref)])
			    (set-ptr! p x)
			    (loop (mk-left x)))]
		    [(+1) (let ([x (copy ref)])
			    (set-ptr! p x)
			    (loop (mk-right x)))]
		    [else (ic-error 'extend-fmap
				    "Unexpected fmap-compare value ~a"
				    z)]))]))))
      (define (copy t)
	(variant-case t
	  [treap-empty () t]
	  [treap-node (key verbatim value hash left right)
            (make-treap-node key verbatim value hash left right)]))
      (define (insert)
	(let* ([h (fmap-random!)]
	       [kv (make-treap-node mk k v h (empty-fmap) (empty-fmap))])
	  (let loop ([p (mk-addr)])
	    (let ([ref (ref-ptr p)])
	      (variant-case ref
		[treap-empty () (set-ptr! p kv) fm]
		[treap-node (key hash)
	          (cond
		   [(fl<? h hash)
		    (let reorder ([lf (mk-left kv)] [rt (mk-right kv)]
				  [q (begin (set-ptr! p kv) (copy ref))])
		      (variant-case q
			[treap-empty () (set-ptr! lf (empty-fmap))
				        (set-ptr! rt (empty-fmap))
					fm]
			[treap-node (key left right)
		          (let ([z (fmap-compare mk key)])
			    (case z
			      [(-1) (set-ptr! rt q)
			            (reorder lf (mk-left q) (copy left))]
			      [(+1) (set-ptr! lf q)
			            (reorder (mk-right q) rt (copy right))]
			      [else (ic-error 'extend-fmap
					      "can't happen (reorder)")]))]))]
		   [else (let ([z (fmap-compare mk key)]
			       [x (copy ref)])
			   (set-ptr! p x)
			   (case z
			     [(-1) (loop (mk-left x))]
			     [(+1) (loop (mk-right x))]
			     [else (ic-error 'extend-fmap
					     "can't happen (loop)")]))])])))))
  (lookup-fmap fm k update insert))))

(define (fmap->alist fm)
  (let loop ([r* '()] [fm fm])
    (variant-case fm
      [treap-empty () r*]
      [treap-node (verbatim value left right)
	(loop (cons (cons verbatim value) (loop r* left)) right)])))

(define (fmap-for-each fm filter proc)
  (let loop ([fm fm])
    (variant-case fm
      [treap-empty () #f]
      [treap-node (verbatim value left right)
	(loop left)
	(if (filter verbatim value) (proc verbatim value))
	(loop right)])))
