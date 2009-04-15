(require "./ch02-6")
(require "./ch19-2")

(define (rewrites category)
  "Return a list of the possible rewrites for this category"
  #;(format #t "  (rule-rhs (assoc ~a *grammar*)))\n" category)
  ;;(format #t "(rewrites ~a)..\n" category)
;  (rule-rhs (assoc category *grammar*)))
  (cl:thru (filter-map (lambda (e)
						 ;;(format #t "- e:~a\n" e)
						 (and (eq? category (rule-lhs e)) (rule-rhs e)))
					   *grammar*) ))

(define (generate phrase)
  "Generate a random sentence or phrase"
;  (format #t "(generate ~a)..\n" phrase)
  (cond [(cl:listp phrase)
		 ;(format #t "(mappend generate ~a)\n" phrase)
		 (mappend generate phrase)]
		[(rewrites phrase)
		 (generate (random-elt (rewrites phrase)))]
		[else (list phrase)]
		))


(define-macro (memoize fn . args)
  (let-keywords* args ((test 'eqv?))
	;(print test)
	(let ([memoized-fn (gensym)]
		  [orig-fn (gensym)]
		  [ht (gensym)]
		  [clear-hash-fn
		   (string->symbol (string-append "clear-memoize-of-" (symbol->string fn) "!"))])
	  `(begin
		 (define ,ht (make-hash-table ',test))
		 (define ,orig-fn ,fn)
		 (define ,memoized-fn
		   (lambda (arg)
			 (or (hash-table-get ,ht arg #f)
				 (let1 val (,orig-fn arg)
				   (hash-table-put! ,ht arg val)
				   val))))
		 (define ,fn ,memoized-fn)
		 (define ,clear-hash-fn (lambda () (hash-table-clear! ,ht)))
		 )
	  )))

#|
(memoize lexical-rules)
(memoize rules-starting-with)
(memoize parse :test eq?)

(define (parser words)
  "Return all complete parses of a list of words."
  ;(clear-memoize 'parse) ;**
  (clear-memoize-of-parse!)
  (map parse-tree (complete-parses (parse words))))
|#

(define (use-grammar grammar)
  "Switch to a new grammar."
  (clear-memoize-of-rules-starting-with!)
  (clear-memoize-of-lexical-rules!)
  (length (set! *grammar* grammar)))
