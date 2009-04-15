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

(dotimes (i 10)
  (print (sentence))
  )
