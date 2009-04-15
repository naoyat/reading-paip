(require "./ch08-4")

(use gauche.test)
(test-start "§ 8.5")
;(simplifier)
(define-macro (simp-test expr expected) (test* #`",expr" expected (simp expr)))

(test-section "p.251")
(simp-test '(x + y + y + x)
           '(x + (y + (y + x))))

(simp-test '(3 * x + 4 * x)
           '((3 * x) + (4 * x)) )
(simp-test '(3 * x + y + x + 4 * x)
           '((3 * x) + (y + (x + (4 * x)))) )

(test-end)
