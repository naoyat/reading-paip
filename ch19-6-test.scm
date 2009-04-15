(require "./ch19-6")

(use gauche.test)
(test-start "§ 19.6")

(memoize lexical-rules)
(memoize rules-starting-with)
(memoize parse :test eq?)

(define-macro (test-set=* name expected expr)
  `(test* ,name ,expected ,expr
		  (lambda (a b) (lset= equal? a b))))

;(test-section "grammar #1: meanings")
(use-grammar
 '((NP   -> (NP CONJ NP) infix-funcall     infix-scorer)
   (NP   -> (N P N)      infix-funcall     infix-scorer)
   (NP   -> (N)          list)
   (NP   -> (【 NP 】)   arg2)
   (NP   -> (NP ADJ)     rev-funcall       rev-scorer)
   (NP   -> (NP OP N)    infix-funcall)
   (N    -> (D)          identity)
   (N    -> (N D)        10*N+D)
   (P    -> to           integers          prefer<)
   (【   -> 【           【)
   (】   -> 】           】)
   (OP   -> repeat       repeat)
   (CONJ -> and          append            prefer-disjoint)
   (CONJ -> without      cl:set-difference prefer-subset)
   (ADJ  -> reversed     reverse           inv-span)
   (ADJ  -> shuffled     permute           prefer-not-singleton)
   (D -> 1 1) (D -> 2 2) (D -> 3 3) (D -> 4 4) (D -> 5 5)
   (D -> 6 6) (D -> 7 7) (D -> 8 8) (D -> 9 9) (D -> 0 0)))

;;;
(test-section "19.6")
(all-parses '(1 to 6 without 3 and 4))
(all-parses '(1 and 3 to 7 and 9 without 5 and 6))
(all-parses '(1 and 3 to 7 and 9 without 5 and 2))

(test-section "meaning")
(test-set=* "1 to 5 without 3 and 4" '(1 2 5) (meaning '(1 to 5 without 3 and 4)))
(test-set=* "1 to 5 without 3 and 6" '(1 2 4 5 6) (meaning '(1 to 5 without 3 and 6)))
(test-set=* "1 to 5 without 3 and 6 shuffled" '(6 4 1 2 5) (meaning '(1 to 5 without 3 and 6 shuffled)))
(test-set=* "【 1 to 5 without 【 3 and 6 】 】 reversed" '(5 4 2 1) (meaning '(【 1 to 5 without 【 3 and 6 】 】 reversed)))
(test-set=* "1 to 5 to 9" '() (meaning '(1 to 5 to 9)))
;(test-set=* "1 to 5 without 3 and 7 repeat 2" '() 
(meaning '(1 to 5 without 3 and 7 repeat 2))

(all-parses '(1 to 5 without 3 and 7 repeat 2))


(test-end)

