(require "./ch19-3")

(define s (generate 'S))
(print "s: " s)
(print (parser s))
(time (length (parser s)))

(memoize lexical-rules)
(memoize rules-starting-with)
(memoize parse :test eq?)

(use-grammar *grammar4*)
(print (parser s))
(time (length (parser s)))
