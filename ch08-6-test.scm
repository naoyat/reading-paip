(require "./ch08-6")

(use gauche.test)
(test-start "§ 8.6")

(test-section "free-of")
(test* "(^ 2 3), of x" #t (free-of '(^ 2 3) 'x))
(test* "(^ x 3), of x" #f (free-of '(^ x 3) 'x))
(test* "(* 3 (^ x 3)), of x" #f (free-of '(* 3 (^ x 3)) 'x))
(test* "(^ y 3), of x" #t (free-of '(^ y 3) 'x))
(test* "(* 3 (^ y 3)), of x" #t (free-of '(* 3 (^ y 3)) 'x))

(test-section "find-anywhere")
(test* "find x in (^ 8 1)" #f
	   (find-anywhere 'x '(^ 8 1)))
(test* "find x in (^ (+ (^ x 3) 2) -3)" 'x
	   (find-anywhere 'x '(^ (+ (^ x 3) 2) -3)))
(test* "find x in (^ x 2)" 'x
	   (find-anywhere 'x '(^ x 2)))

(test-section "length=1?")
(test* "()" #f (length=1? '()))
(test* "(1)" #t (length=1? '(1)))
(test* "(1 2)" #f (length=1? '(1 2)))
(test* "(1 2 3)" #f (length=1? '(1 2 3)))
(test* "x" #f (length=1? 'x))


(read-line)
;(simplifier)
(define-macro (simp-test expr expected) (test* #`",expr" expected (simp expr)) (read-line))

(test-section "p.258")
(simp-test '(Int x * sin(x ^ 2) d x)
           '(1/2 * (- (cos (x ^ 2)))) )
(simp-test '(Int ((3 * x ^ 3) - 1 / (3 * x ^ 3)) d x)
           '((3 * ((x ^ 4) / 4)) - (1/3 * ((x ^ -2) / -2))) )
(simp-test '(Int (3 * x + 2) ^ -2/3 d x)
           '(((3 * x) + 2) ^ 1/3) )
(simp-test '(Int sin(x) ^ 2 * cos(x) d x)
           '(((sin x) ^ 3) / 3) )
(simp-test '(Int sin(x) / (1 + cos(x)) d x)
           '(-1 * (log ((cos x) + 1))) )
(simp-test '(Int (2 * x + 1) / (x ^ 2 + x - 1) d x)
           '(log ((x ^ 2) + (x - 1))) )
(simp-test '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)
           '(8 * ((1/3 * (((x ^ 3) + 2) ^ -2)) / -2)) )

(test-section "p.259")
(set-simp-fn 'Int (lambda (exp)
                    (unfactorize (factorize (integrate (exp-lhs exp) (exp-rhs exp))))))
(simp-test '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)
           '(-4/3 * (((x ^ 3) + 2) ^ -2)) )

(test-end)
