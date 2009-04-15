(require "./cl-emu")
;;

(define (sentence) (append (noun-phrase) (verb-phrase)))
(define (noun-phrase) (append (Article) (Noun)))
(define (verb-phrase) (append (Verb) (noun-phrase)))
(define (Article) (one-of '(the a)))
(define (Noun) (one-of '(man ball woman table)))
(define (Verb) (one-of '(hit took saw liked)))

(define (one-of set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))
(define (random-elt choices)
  "Choose an element from a list at random."
  (cl:elt choices (cl:random (length choices))))

;;
(define (Adj*)
  (if (= 0 (cl:random 2))
	  cl:nil
	  (append (Adj) (Adj*))))
#|
(define (Adj*)
  "Warning - incorrect definition of Arjectives."
  (one-of '(nil (append (Adj) (Adj*)))))
(define (Adj*)
  "Warning - incorrect definition of Arjectives."
  (one-of (list nil (append (Adj) (Adj*)))))
|#
(define (PP*)
  (if (random-elt '(#t #f))
	  (append (PP) (PP*))
	  cl:nil))
(define (noun-phrase) (append (Article) (Adj*) (Noun) (PP*)))
(define (PP) (append (Prep) (noun-phrase)))
(define (Adj) (one-of '(big little blue green adiabatic)))
(define (Prep) (one-of '(to in by with on)))
	  
;;(dotimes (i 10)
(print (sentence))
;;)
