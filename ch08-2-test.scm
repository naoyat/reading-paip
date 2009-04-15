(require "./ch08-2")

(use gauche.test)
(test-start "§ 8.2")
;(simplifier)
(define-macro (simp-test expr expected) (test* #`",expr" expected (simp expr)))

(simp-test '(2 + 2)
           '4)
(simp-test '(5 * 20 + 30 + 7)
           '137)
(simp-test '(5 * x - (4 + 1) * x)
           '0)
(simp-test '(y / z * (5 * x - (4 + 1) * x))
           '0)
(simp-test '((4 - 3) * x + (y / y - 1) * z)
           'x)
(simp-test '(1 * f(x) + 0)
           '(f x))
(simp-test '(3 * 2 * x)
           '(3 * (2 * x)))
(simp-test '(3 - 2 - x)
           '(3 - (2 - x)))

(test-end)