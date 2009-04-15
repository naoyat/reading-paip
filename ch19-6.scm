(require "./ch19-5")

(define (make-rule lhs rhs . args)
  (let-optionals* args ((sem #f)
						(score #f))
	(list lhs '-> rhs sem score)))
(define (->rule lis) lis)

(define (rule-lhs rule) (first rule))
(define (rule-rhs rule) (third rule))
(define (rule-sem rule) (cl:thru (cl:fourth rule)))
(define (rule-score rule) (cl:thru (cl:fifth rule)))

#;(define (lexical-rules word)
  "Return a list of rules with word on the right-hand side."
  (or (cl:thru (find-all word *grammar* :key rule-rhs :test equal?))
	  (map (lambda (cat) `(,cat -> ,word ,word)) *open-categories*)))

(define (new-tree lhs sem score rhs) (list lhs sem score rhs))
(define (tree-lhs tree) (first tree))
(define (tree-sem tree) (second tree))
(define (tree-sem-set! tree sem)
  ;(format #t "(tree-sem-set! ~a := ~a)\n" tree sem)
  (set-car! (cdr tree) sem)
  sem)
(define (tree-score tree) (third tree))
(define (tree-score-set! tree score)
  ;(format #t "tree-score-set! of tree:~a to score:~a\n" tree score)
  (set-car! (cddr tree) score)
  ;(format #t "  ==> ~a\n" tree)
  score)
(define (tree-rhs tree) (fourth tree))

(define (use-grammar grammar)
  "Switch to a new grammar."
  (clear-memoize-of-rules-starting-with!)
  (clear-memoize-of-lexical-rules!)
  (length (set! *grammar*
;				(map (cut apply make-rule <>) grammar))))
				grammar)))

(define (parse words)
  "Bottom-up parse, returning all parses of any prefix of words.
   This version has semantics and preference scores."
  ;(format #t "(parse words:~a)\n" words)
  ;(format #t "(parse words:~a)\n" words)
  (cl:unless (null? words)
			 (cl:mapcan (lambda (rule)
						  ;(format #t "%% rule:~a words:~a\n" rule words)
						  (extend-parse (rule-lhs rule)
										(rule-sem rule)
										(rule-score rule)
										(list (car words)) ;***
										(cdr words)
										'()))
						(lexical-rules (car words)))))

(define (extend-parse lhs sem score rhs rem needed) ;***
  "Look for the categories needed to complete the parse.
  This version has semantics and preference scores."
  ;(format #t "(extend-parse lhs:~a, sem:~a, score:~a, rhs:~a, rem:~a, needed:~a)\n" lhs sem score rhs rem needed)
  (if (null? needed)
	  ;; If nothing is needed, return this parse and upward extensions,
	  ;; unless the semantics fails
	  (let1 parse (make-parse (new-tree lhs sem score rhs) ;***
							  rem)
		(cl:unless (null? (apply-semantics (parse-tree parse)))
				   (apply-scorer (parse-tree parse)) ;**
				   (cons parse
						 (cl:mapcan (lambda (rule)
									  (extend-parse (rule-lhs rule)
													(rule-sem rule)
													(rule-score rule)
													(list (parse-tree parse)) ;***
													rem
													(cdr (rule-rhs rule))))
									(rules-starting-with lhs)))))
	  ;; Otherwise try to extend rightward
	  (cl:mapcan (lambda (p)
				   (cl:if (eq? (parse-lhs p) (car needed))
						  (extend-parse lhs sem score
										(append1 rhs (parse-tree p)) ;***
										(parse-rem p) (cdr needed))))
				 (parse rem))))

(define (apply-scorer tree)
  "Compute the score for this tree."
  (let1 score (or (tree-score tree) 0)
	(tree-score-set! tree
					 (if (terminal-tree? tree)
						 score
						 ;; Add up the constituent's scores,
						 ;; along with the tree's score
						 (+ (sum (tree-rhs tree) tree-score-or-0)
							(if (number? score)
								score
								(or (cl:apply score (tree-rhs tree)) 0)))))))

(define (tree-score-or-0 tree)
  (let1 score (tree-score tree)
	(if (number? score) score 0)))
;  (if (number? (tree-score tree))
;	  (tree-score tree)
;	  0))

;;;
(define (prefer< x y)
  (if (>= (sem x) (sem y)) -1 #f))
(define (prefer-disjoint x y)
  ;(cl:thru (cl:if (lset-intersection equal? (sem x) (sem y)) -1.0))
  (if (cl:thru (lset-intersection equal? (sem x) (sem y))) -1.0 #f))
(define (prefer-subset x y)
  (+ (inv-span x) (if (cl:subsetp (sem y) (sem x)) 0 -3)))
(define (prefer-not-singleton x)
  (+ (inv-span x) (if (< (length (sem x)) 2) -4 0)))

(define (infix-scorer arg1 scorer arg2)
  (funcall (tree-score scorer) arg1 arg2))
(define (rev-scorer arg scorer)
  (funcall (tree-score scorer) arg))

(define (arg2 a1 a2 . a-n) a2)
(define (rev-funcall arg function) (funcall function arg))
(define (repeat lis n)
  "Append list n times."
  (if (= n 0)
	  '()
	  (append lis (repeat lis (- n 1)))))

(define (span-length tree)
  "How many words are in tree?"
  (if (terminal-tree? tree)
	  1
	  (sum (tree-rhs tree) span-length)))

(define (inv-span tree) (/ 1 (span-length tree)))
(define (sem tree) (tree-sem tree))

(define (integers start end)
  "A list of all the integers in the range [start...end] inclusive.
  This version allows start > end."
  (cond [(< start end) (cons start (integers (+ start 1) end))]
		[(> start end) (cons start (integers (- start 1) end))]
		[else (list start)]))

(define (sum numbers . args)
  "Sum the numbers, or sum (map fn numbers)."
  (let-optionals* args ((fn #f))
	(if fn
		(fold (lambda (a e) (+ (funcall fn a) e)) 0 numbers)
		(apply + numbers))))

(define (permute bag)
  "Return a random permutation of the given input list."
  (if (null? bag)
	  '()
	  (let1 e (random-elt bag)
		(cons e (permute (cl:remove e bag :count 1 :test eq?))))))

(define (all-parses words)
  (define (%3.1f x)
	(let1 sign (if (<= 0 x) "" "-")
	  (receive (f int) (modf (abs x))
		(let ([i. (inexact->exact int)]
			  [f. (round->exact (* f 10))])
		  (format "~5@a" (format "~a~d.~d" sign i. f.))))))

  (format #t "\nScore  Semantics  ~25@a\n" words)
  (format #t "=====  =========  ~25@a\n" "============================")
  (for-each (lambda (tree)
			  (format #t "~5a  ~9a  ~25@a\n"
					  (%3.1f (tree-score tree)) (tree-sem tree) (bracketing tree)))
			(cl:sort (parser words) > :key tree-score) ))

(define (bracketing tree)
  "Extract the terminals, bracketed with parens."
  (cond [(cl:atom tree) tree]
		[(length=1? (tree-rhs tree))
		 (bracketing (car (tree-rhs tree)))]
		[else (map bracketing (tree-rhs tree))]))

;;;;;;;;;;
(define (meaning words . args)
  "Choose the single top-ranking meaning for the words."
  (let-optionals* args ((tie-breaker query-user))
	(let* ([trees (cl:sort (parser words) > :key tree-score)]
;		   [_0 (format #t "- trees: ~a\n" trees)]
		   [best-score (cl:if trees (tree-score (car trees)) 0)]
;		   [_1 (format #t "- best-score: ~a\n" best-score)]
		   [best-trees (cl:delete best-score trees :key tree-score :test-not eqv?)]
;		   [_2 (format #t "- best-trees: ~a\n" best-trees)]
		   [best-sems (cl:delete-duplicates (map tree-sem best-trees) :test equal?)]
;		   [_3 (format #t "- best-sems: ~a\n" best-sems)]
		   )
	  (case (length best-sems)
		[(0) (format #t "Sorry, I didn't understand that.") cl:nil]
		[(1) (car best-sems)]
		[else (funcall tie-breaker best-sems)]))))

(define *query-io* #t)

(define (query-user choices . args)
  "Ask user to make a choice."
  (let-optionals* args ([header-str "\nPlease pick one:"]
						[footer-str "\nYour choice? "])
	(format *query-io* header-str)
	(let loop ((choices choices) (i 1))
	  (unless (null? choices)
		(let1 choice (car choices)
		  (format *query-io* "\n~3d: ~a" i choice)
		  (loop (cdr choices) (+ i 1)))))
	(format *query-io* footer-str)
	(cl:nth (- (read) 1) choices)))

