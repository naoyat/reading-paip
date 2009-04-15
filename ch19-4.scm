;(require "./ch19-2")
(require "./ch19-3")

(define *open-categories* '(N V A Name)); defparameter
;; Categories to consider for unknown words

(define (lexical-rules word)
  "Return a list of rules with word on the right-hand side."
  (or (cl:thru (find-all word *grammar* :key rule-rhs :test equal?))
	  (map (lambda (cat) `(,cat -> ,word)) *open-categories*)))

(parser '(John liked Mary))

