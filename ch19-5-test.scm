(require "./ch19-5")

(use gauche.test)
(test-start "§ 19.5")

(memoize lexical-rules)
(memoize rules-starting-with)
(memoize parse :test eq?)

(test-section "trivial")
(test* "(list 1)" '(1) (list 1))
(test* "(identity 1)" 1 (identity 1))

(define-macro (test-set=* name expected expr)
  `(test* ,name ,expected ,expr
		  (lambda (a b) (map (cut lset= equal? <> <>) a b))))
;;
(test-section "grammar #1: meanings")
(use-grammar
 '((NP   -> (NP CONJ NP) infix-funcall)
   (NP   -> (N)          list)
   (NP   -> (N P N)      infix-funcall)
   (N    -> (DIGIT)      identity)
   (P    -> to           integers)
   (CONJ -> and          cl:union)
   (CONJ -> without      cl:set-difference)
   (N -> 1 1) (N -> 2 2) (N -> 3 3) (N -> 4 4) (N -> 5 5)
   (N -> 6 6) (N -> 7 7) (N -> 8 8) (N -> 9 9) (N -> 0 0)))

(test-set=* "1 to 5 without 3"
	   '((1 2 4 5))
	   (meanings '(1 to 5 without 3)))
(test-set=* "1 to 4 and 7 to 9"
	   '((1 2 3 4 7 8 9))
	   (meanings '(1 to 4 and 7 to 9)))
(test-set=* "1 to 6 without 3 and 4"
	   '((1 2 4 5 6) (1 2 5 6))
	   (meanings '(1 to 6 without 3 and 4)))

;;
(test-section "grammar #2: meanings")
(use-grammar
 '((NP -> (NP CONJ NP) infix-funcall)
   (NP -> (N)          list)
   (NP -> (N P N)      infix-funcall)
   (N  -> (DIGIT)      identity)
   (N  -> (N DIGIT)    10*N+D)
   (P  -> to           integers)
   (CONJ -> and        union*)
   (CONJ -> without    set-diff)
   (DIGIT -> 1 1) (DIGIT -> 2 2) (DIGIT -> 3 3)
   (DIGIT -> 4 4) (DIGIT -> 5 5) (DIGIT -> 6 6)
   (DIGIT -> 7 7) (DIGIT -> 8 8) (DIGIT -> 9 9)
   (DIGIT -> 0 0)))

(test-set=* "1 to 6 without 3 and 4"
	   '((1 2 5 6))
	   (meanings '(1 to 6 without 3 and 4)))
(test-set=* "1 and 3 to 7 and 9 without 5 and 6"
	   '((1 3 4 7 9))
	   (meanings '(1 and 3 to 7 and 9 without 5 and 6)))
(test-set=* "1 and 3 to 7 and 9 without 5 and 2"
	   '((1 3 4 6 7 9 2))
	   (meanings '(1 and 3 to 7 and 9 without 5 and 2)))
(test-set=* "1 9 8 to 2 0 1"
	   '((198 199 200 201))
	   (meanings '(1 9 8 to 2 0 1)))
(test-set=* "1 2 3"
	   '(123 (123))
	   (meanings '(1 2 3)))

(test-end)

