(use gauche.test)

(test-start "auxfns")
(require "./auxfns")

(test-section "find-all")
(test* "'a in '(a b d a)"
	   '(a a)
	   (find-all 'a '(a b d a)))
(test* "'a in cars of '((a b) (b a) (d a) (a e))"
	   '((a b) (a e))
	   (find-all 'a '((a b) (b a) (d a) (a e)) :key car))
(test* "'(a 1) in cars of '((a b) (b a) (d a) (a e)), with equal?"
	   '(((a 1) b))
	   (find-all '(a 1) '(((a 1) b) (b (a 2)) (d (a 3)) ((a 4) e)) :key car :test equal?))

(test-end)
