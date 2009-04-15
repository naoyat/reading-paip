(require "./ch19-1")

(use gauche.test)
(test-start "§ 19.1")

(define *grammar1* '((Sentence -> (NP VP)) ;defparameter
                     (NP -> (Art Noun))
                     (VP -> (Verb NP))
                     (Art -> the a)
                     (Noun -> man ball woman table)
                     (Verb -> hit took saw liked)))

(define *grammar3* ;defparameter
  '((Sentence -> (NP VP))
    (NP -> (Art Noun))
    (VP -> (Verb NP))
    (Art -> the) (Art -> a)
    (Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table)
    (Noun -> noun) (Noun -> verb)
    (Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked)))

(set! *grammar* *grammar3*)

(test-section "parser")
(test* "the table"
       '((NP (Art the) (Noun table)))
       (parser '(the table)))
(test* "the ball hit the table"
       '((Sentence (NP (Art the) (Noun ball))
                   (VP (Verb hit)
                       (NP (Art the) (Noun table)))))
       (parser '(the ball hit the table)))
(test* "the noun took the verb"
       '((Sentence (NP (Art the) (Noun noun))
                   (VP (Verb took)
                       (NP (Art the) (Noun verb)))))
       (parser '(the noun took the verb)))

(test-end)

