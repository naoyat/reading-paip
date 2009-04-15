(require "./ch07-1")

(use gauche.test)
(test-start "7.1--7.2")

(test-section "7.1")
(test* "If z is 3, what is twice z" 6 (student '(If z is 3, what is twice z)))

(test-end)
