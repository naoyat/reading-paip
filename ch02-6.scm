(require "./ch02-5")

(define (generate-tree phrase)
  "Generate a random sentence or phrase, with a complete parse tree."
  (cond [(listp phrase) (mapcar generate-tree phrase)]
		[(rewrites phrase)
		 => (lambda (choices)
			  (cons phrase
					(generate-tree (random-elt choices))))]
		[else (list phrase)]
		))

;;(print (generate-tree 'sentence))

(define (generate-all phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond [(null? phrase) (list '())]
		[(listp phrase) (combine-all (generate-all (first phrase))
									 (generate-all (rest phrase)))]
		[(rewrites phrase)
		 => (lambda (choices)
			  (mappend generate-all choices))]
		[else (list (list phrase))] ))
(define (combine-all xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))." ;; 何てcartesian product?
  (mappend (lambda (y)
			 (map (lambda (x) (append x y)) xlist))
		   ylist))

;(use util.combinations)
;(define combine-all cartesian-product)

;(set! *grammar* *simple-grammar*)
#|
(print (generate-all 'Article))
(print (generate-all 'Noun))
;(print (generate-all 'noun-phrase))
#?=(length (generate-all 'sentence))
|#


;;Exercise 2.4
(define (cross-product fn xlist ylist)
  (append-map (lambda (y) (map (cut fn <> y) xlist))
			  ylist))
;		 (map (lambda (y) (map (cut fn <> y) xlist))
;			  ylist)))

(define (combine-all xlist ylist)
  (cross-product append xlist ylist))
