(require "./cl-emu")

(define (mappend fn the-list)
  (apply append (map fn the-list)))
;;;;;;;

(define (one-of set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(define (random-elt choices)
  "Choose an element from a list at random."
  (if choices
	  (cl:elt choices (cl:random (length choices)))
	  #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
	(noun-phrase -> (Article Noun))
	(verb-phrase -> (Verb noun-phrase))
	(Article -> the a)
	(Noun -> man ball woman table)
	(Verb -> hit took saw liked))
  #;"A grammar for a trivial subset of English.")

(define *grammar* *simple-grammar*
  #;"The grammar used by generate.  Initially, this is *simple-grammar*, but we can switch to other grammars.")

;;#?=(assoc 'Noun *grammar*)

(define (rule-lhs rule)
  "The lhs of a rule"
  #;(format #t "(rule-lhs ~a) => ~a\n" rule (first rule))
  (if rule
	  (first rule)
	  #f))

(define (rule-rhs rule)
  "The rhs of a rule"
  #;(format #t "(rule-rhs ~a) => ~a\n" rule (cl:rest (cl:rest rule)))
  (if rule
	  (cl:rest (cl:rest rule))
	  #f))

(define (rewrites category)
  "Return a list of the possible rewritesfor this category"
  #;(format #t "  (rule-rhs (assoc ~a *grammar*)))\n" category)
  #;(format #t "(rewrites ~a)..\n" category)
  (rule-rhs (assoc category *grammar*)))

(define (generate phrase)
  "Generate a random sentence or phrase"
  #;(format #t "(generate ~a)..\n" phrase)
  (cond [(cl:listp phrase)
		 ;(format #t "(mappend generate ~a)\n" phrase)
		 (mappend generate phrase)]
		[(rewrites phrase)
		 (generate (random-elt (rewrites phrase)))]
		[else (list phrase)]
		))
#|
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'noun-phrase))
(print (generate 'verb-phrase))
|#
(define (generate phrase)
  ""
  (if (cl:listp phrase)
	  (mappend generate phrase)
	  (let1 choices (rewrites phrase)
		(if choices
			(generate (random-elt choices))
			(list phrase)
			))))
#|
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'noun-phrase))
(print (generate 'verb-phrase))
|#
;;exercise 2.1
(define (generate phrase)
  (cond [(pair? phrase) (mappend generate phrase)]
		[(rewrites phrase) => (lambda (choices) (generate (random-elt choices)) )]
		[else (list phrase)]
		))
#|
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'noun-phrase))
(print (generate 'verb-phrase))
|#

;;(rewrites category) => (rule-rhs (assoc category *grammar*)))

;;exercise 2.2
(define (generate phrase)
  (cond [(pair? phrase) ;list
		 (mappend generate phrase)]
		[(assoc phrase *grammar*) => ;nonterminal
		 (lambda (rhs) (generate (random-elt (rule-rhs rhs))))]
		[else ;terminal
		 (list phrase)]
		))
#|
(print (generate 'sentence))
(print (generate 'sentence))
(print (generate 'noun-phrase))
(print (generate 'verb-phrase))
|#