(require "./ch19-2")

(use gauche.test)
(test-start "§ 19.2")

(test-section "parser")
(test* "the man hit the table with the ball"
       '((S (NP (D the) (N man))
            (VP (VP (V hit) (NP (D the) (N table)))
                (PP (P with) (NP (D the) (N ball)))))
         (S (NP (D the) (N man))
            (VP (V hit)
                (NP (NP (D the) (N table))
                    (PP (P with) (NP (D the) (N ball)))))) )
       (parser '(the man hit the table with the ball)) )
(test* "the orange saw"
       '((S (NP (D the) (N orange)) (VP (V saw)))
         (NP (D the) (A+ (A orange)) (N saw)))
       (parser '(the orange saw)))

(test-end)

