;(require "./ch19-3") ;; no modified lexical-rules
(require "./ch19-4")
;(require "./cl-emu")
;(use srfi-1)
;(require "./ch08-6") ;length=1?
(define (length=1? x)
  (and (pair? x) (null? (rest x))))


(define (make-rule lhs rhs sem) (list lhs '-> rhs sem))
(define (rule-lhs rule) (first rule))
(define (rule-rhs rule) (third rule))
(define (rule-sem rule) (fourth rule))

(define (new-tree lhs sem rhs) (list lhs sem rhs))
(define (tree-lhs tree) (first tree))
(define (tree-sem tree) (second tree))
(define (tree-sem-set! tree sem)
  ;(format #t "~a, s/~a/~a/" tree (tree-sem tree) sem)
  (set-cdr! tree (cons sem (cddr tree)))
  ;(format #t " => ~a\n" tree)
  sem)

(define (tree-rhs tree) (third tree))
(define (tree-desc tree)
  (format "#T(lhs:~a sem:~a rhs:~a)"
		  (tree-lhs tree)
		  (tree-sem tree)
		  (tree-rhs tree)))

;(NP -> (NP CONJ NP) infix-funcall)

(define (integers start end)
  "A list of all the integers in the range [start...end] inclusive."
  (if (> start end) '()
	  (cons start (integers (+ start 1) end))))

(define (infix-funcall arg1 function arg2)
  "Apply the function to the two arguments"
  (funcall function arg1 arg2))

(define (parse words)
  "Bottom-up parse, returning all parses of any prefix of words.
   This version has semantics."
  (cl:unless (null? words)
			 (cl:mapcan (lambda (rule)
						  (extend-parse (rule-lhs rule)
										(rule-sem rule)
										(list (car words))
										(cdr words)
										'()))
						(lexical-rules (car words)))))

(define (extend-parse lhs sem rhs rem needed)
  "Look for the categories needed to complete the parse.
   This version has semantics."
  (if (null? needed)
	  ;; If nothing is needed, return this parse and upward extensions,
	  ;; unless the semantics fails
	  (let1 parse (make-parse (new-tree lhs sem rhs)
							  rem)
		(cl:unless (null? (apply-semantics (parse-tree parse)))
				   (cons parse
						 (cl:mapcan (lambda (rule)
									  (extend-parse (rule-lhs rule)
													(rule-sem rule)
													(list (parse-tree parse))
													rem
													(cdr (rule-rhs rule))))
									(rules-starting-with lhs)))))
	  ;; otherwise try to extend rightward
	  (cl:mapcan (lambda (p)
				   (cl:if (eq? (parse-lhs p) (car needed))
						  (extend-parse lhs sem
										(append1 rhs (parse-tree p))
										(parse-rem p) (cdr needed))))
				 (parse rem))))

(define (apply-semantics tree)
  "For terminal nodes, just fetch the semantics.
   Otherwise, apply the sem function to its constituents."
;  (format #t "(apply-semantics ~a)\n" (tree-desc tree))
  (if (terminal-tree? tree)
	  (tree-sem tree)
	  (tree-sem-set! tree
					 (cl:apply (tree-sem tree)
							   (map tree-sem (tree-rhs tree))))))
;	  (set! (tree-sem tree)
;			(apply (tree-sem tree)
;				   (map tree-sem (tree-rhs tree))))))

(define (terminal-tree? tree)
  "Does this tree have a single word on the rhs?"
  (and (length=1? (tree-rhs tree))
	   (cl:atom (car (tree-rhs tree)))))

(define (meanings words)
  "Return all possible meanings of a phrase.  Throw away the syntactic part."
  (cl:remove-duplicates (map tree-sem (parser words)) :test equal?))

;;
(define (union* x y) (if (null? (lset-intersection equal? x y))
						 (append x y)
						 '()))
(define (set-diff x y) (if (cl:subsetp y x) (cl:set-difference x y) '()))
(define (10*N+D n d) (+ (* 10 n) d))
