(require "./ch11-prolog1")
;(require "./ch11-2")

(use gauche.test)
(test-start "§ 11.2")

;;
;; for buggy version
;;
(test-section "p.352 - pat-match")
(test* "" '((?y . 1) (?x . 2)) (pat-match '(?x + ?y) '(2 + 1)) )
#|
(test-section "p.352 - unify<0>")
(test* "" '((?y . 1) (?x . 2)) (unify0 '(?x + 1) '(2 + ?y)) )
(test* "" '((?x . ?y)) (unify0 '(f ?x) '(f ?y)) )
(test* "" '((?y . 0) (?x . ?y) (?a . ?x)) (unify0 '(?a + ?a = 0) '(?x + ?y = ?y)) )

;(test* "" '(0 + 0 = 0) (unifier '(?a + ?a = 0) '(?x + ?y = ?y)) )
;(test* "" '(2 + 2 = 2) (unifier '(?a + ?a = 2) '(?x + ?y = ?y)) )

(test-section "p.354 - unify<0> is buggy!")
(test* "" '((?y . 1) (?x . 2)) (unify0 '(?x + 1) '(2 + ?y)) ) ;; again
(test* "" '((?x . ?y)) (unify0 '?x '?y))
(test* "" '((?y . ?y) (?x . ?y)) (unify0 '(?x ?x) '(?y ?y)) )
;(test* "error" '() (unify '(?x ?x ?x) '(?y ?y ?y)))

(test-section "p.355 - unify<1>")
(test* "" '((?x . ?y)) (unify1 '(?x ?x) '(?y ?y)))
(test* "" '((?x . ?y)) (unify1 '(?x ?x ?x) '(?y ?y ?y)))
(test* "" '((?y . ?x) (?x . ?y)) (unify1 '(?x ?y) '(?y ?x)))
;(test* "" '() (unify1 '(?x ?y a) '(?y ?x ?x)))

(test-section "p.355 - unify<2>")
(test* "" '((?x . ?y)) (unify2 '(?x ?x) '(?y ?y)))
(test* "" '((?x . ?y)) (unify2 '(?x ?x ?x) '(?y ?y ?y)))
;(test* "" '((?y . ?x) (?x . ?y)) (unify '(?x ?y) '(?y ?x)))
(test* "" '((?x . ?y)) (unify2 '(?x ?y) '(?y ?x)))
(test* "" '((?y . a) (?x . ?y)) (unify2 '(?x ?y a) '(?y ?x ?x)))
(test* "problem" '((?x f ?x)) (unify2 '?x '(f ?x))) ;; must bean ((?x . ((f ?x))))

(test-section "p.356 - unify<3>")
(test* "" '((?x . ?y)) (unify '(?x ?x) '(?y ?y)))
(test* "" '((?x . ?y)) (unify '(?x ?x ?x) '(?y ?y ?y)))
(test* "" '((?x . ?y)) (unify '(?x ?y) '(?y ?x)))
(test* "" '((?y . a) (?x . ?y)) (unify '(?x ?y a) '(?y ?x ?x)))
(test* "infinite loop" '() (unify '?x '(f ?x))) ;; must bean ((?x . ((f ?x))))
|#
(test-section "p.357")
(test* "" '((?y . a) (?x . ?y)) (unify '(?x ?y a) '(?y ?x ?x)))
(test* "" '() (unify '?x '(f ?x)))
(test* "" '() (unify '(?x ?y) '((f ?y) (f ?x))))
(test* "" '() (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
(test* "" '((#t . #t)) (unify 'a 'a))

(test-section "p.357 - unifier")
(test* "" '(a a a) (unifier '(?x ?y a) '(?y ?x ?x)))
(test* "" '((?a * 5 ^ 2) + (4 * 5) + 3)
	   (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
				'(?z + (4 * 5) + 3)))

(test-section "p.358 - if *occurs-check* is false")
(set! *occurs-check* #f)
(test* "" '((?x f ?x)) (unify '?x '(f ?x)))
(test* "" '((?y f ?x) (?x f ?y)) (unify '(?x ?y) '((f ?y) (f ?x))))
(test* "" '((?z ?x ?y) (?y ?x ?z) (?x ?y ?z)) (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
(set! *occurs-check* #t)

;;
(test-section "")
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(test-end)

#;(test* ""
	   '(((?who . Lee))
		 ((?who . Kim))
		 ((?x2856 . Robin) (?who . ?x2856))
		 ((?x2860 . cats) (?x2857 . cats) (?x2856 . Sandy) (?who . ?x2856))
		 ((?x2865 . cats) (?x2856 . ?x2865) (?who . ?x2856))
		 (??who . Sandy) (?x2867 . Sandy))
	   (?- (likes Sandy ?who)) )

(?- (likes Sandy ?who))

(?- (likes ?who Sandy))

(?- (likes Robin Lee))

(?- (likes ?x ?y) (likes ?y ?x))

(newline)
(?- (member 2 (1 2 3)))
(?- (member 2 (1 2 3 2 1)))

;(?- (member 2 ?list))
;(?- (member ?item ?list))
