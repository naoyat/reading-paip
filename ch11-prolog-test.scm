(require "./prolog")

;(define-macro (?= . goals)
;  `(top-level-prove-val ',(replace-?-vars goals)))
;(define (top-level-prove-val goals)
;  (prove-all `(,@goals); (show-prolog-vars ,@(variables-in goals)))
;			 no-bindings))
;  ;(format #t "\nNo.\n")
;;  (values))
(define (gequal? x y)
  (define (g= x y)
	#;(format #t "comparing ~a : ~a => ~a\n" x y 
			(string=? (x->string x) (x->string y)))
	(string=? (x->string x) (x->string y)))
  (cond [(null? x) (null? y)]
		[(null? y) (null? x)]
		[(atom? x) (if (atom? y) (g= x y) #f)]
		[(atom? y) #f]
		[else (and (gequal? (car x) (car y))
				   (gequal? (cdr x) (cdr y)))]))

(use gauche.test)
(test-start "§ 11.3")

(test-section "p.357")
(test* "(unify (?x ?y a) (?y ?x ?x))"
	   '((?y . a) (?x . ?y))
	   (unify '(?x ?y a) '(?y ?x ?x)))
(test* "(unify ?x (f ?x))"
	   '()
	   (unify '?x '(f ?x)))
(test* "(unify (?x ?y) ((f ?y) (f ?x)))"
	   '()
	   (unify '(?x ?y) '((f ?y) (f ?x))))
(test* "(unify (?x ?y ?z) ((?y ?z) (?x ?z) (?x ?y)))"
	   '()
	   (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
(test* "(unify a a)"
	   '((#t . #t))
	   (unify 'a 'a))

(test-section "p.357 - unifier")
(test* "(unifier (?x ?y a) (?y ?x ?x))"
	   '(a a a)
	   (unifier '(?x ?y a) '(?y ?x ?x)))
(test* "(unifier ((?a * ?x ^ 2) + (?b * ?x) + ?c) (?z + (4 * 5) + 3))"
	   '((?a * 5 ^ 2) + (4 * 5) + 3)
	   (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
				'(?z + (4 * 5) + 3)))

(test-section "p.358 - if *occurs-check* is false")
(set! *occurs-check* #f)
(test* "(unify ?x (f ?x))"
	   '((?x f ?x))
	   (unify '?x '(f ?x)))
(test* "(unify (?x ?y) ((f ?y) (f ?x)))"
	   '((?y f ?x) (?x f ?y))
	   (unify '(?x ?y) '((f ?y) (f ?x))))
(test* "(unify (?x ?y ?z) ((?y ?z) (?x ?z) (?x ?y)))"
	   '((?z ?x ?y) (?y ?x ?z) (?x ?y ?z))
	   (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
(set! *occurs-check* #t)

;;
(test-section "likes who?")
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

;(<- (member ?item (?item . ?rest)))
;(<- (member ?item (?x . ?rest)) (member ?item ?rest))
;(?- (likes Sandy ?who))
;(?- (likes ?who Sandy))
;(?- (likes Robin Lee))
;(?- (likes ?x ?y) (likes ?y ?x))

(test* "?- (likes Sandy ?who)"
	   "?who = Lee\n"
	   (?= (likes Sandy ?who))
	   gequal?)
(test* "?- (likes ?who Sandy)"
	   "?who = Sandy\n"
	   (?= (likes ?who Sandy))
	   gequal?)
(test* "?- (likes Robin Lee)"
	   '()
	   (?= (likes Robin Lee))
	   gequal?)
(test* "?- (likes ?x ?y) (likes ?y ?x)"
	   "?y = Kim\n?x = Sandy\n"
	   (?= (likes ?x ?y) (likes ?y ?x))
	   gequal?)

(test-section "length")
(<- (length () 0))
(<- (length #f 0))
(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

;(test* "?- (length ())" 0 (?= (length ())))
;(test* "?- (length #f)" 0 (?= (length #f)))
;(test* "?- (length (1))" 1 (?= (length (1))))
;(test* "?- (length (1 2))" 2 (?= (length (1 2))))
(test* "(?- (length () ?n)"
	   "?n = 0\n"
	   (?= (length () ?n))
	   )
(test* "(?- (length (a) ?n)"
	   "?n = (1+ 0)\n"
	   (?= (length (a) ?n))
	   )
(test* "(?- (length (a b c d) ?n)"
	   "?n = (1+ (1+ (1+ (1+ 0))))\n"
	   (?= (length (a b c d) ?n))
	   )
#|
* (?- (length ?list (1+ (1+ 0))))

;
?LIST = (?X775 ?X778)
No.
* (?- (length ?list ?n))

?LIST = NIL
;
?N = 0
?LIST = (?X784)
;
?N = (1+ 0)
?LIST = (?X784 ?X787)
;
?N = (1+ (1+ 0))
?LIST = (?X784 ?X787 ?X790)
;
?N = (1+ (1+ (1+ 0)))
?LIST = (?X784 ?X787 ?X790 ?X793)
.
?N = (1+ (1+ (1+ (1+ 0))))
No.
|#

;(?- (length ?list (1+ (1+ 0))))
;(?- (length ?list ?n))

(test-section "member")
(<- (member ?item (?item . ?rest)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

;(?- (member 2 (1 2 3)))
;(?- (member 2 (1 2 3 2 1)))
(test* "?- (member 2 (1 2 3))"
	   "";'((?rest139 . (3)) (?item138 . 2) (?rest137 . (2 3)) (?132135 . 1) (?item136 . 2))
	   (?= (member 2 (1 2 3)))
	   gequal?)
(test* "?- (member 2 (1 2 3 2 1))"
	   "";'((?rest146 . (3 2 1)) (?item145 . 2) (?rest144 2 . (3 2 1)) (?132142 . 1) (?item143 . 2))
	   (?= (member 2 (1 2 3 2 1)))
	   gequal?)

(test* "?- (member ?x (1 2 3))"
	   "?x = 1\n"
	   (?= (member ?x (1 2 3)))
	   gequal?)

;(?- (member 2 ?list))

;(?- (length (a b c d) ?n))
;(?- (length ?l (1+ (1+ 0))) (member a ?l))
;(?- (member a ?l) (length ?l (1+ (1+ 0))))

;(<- (= ?x ?x))

(test-section "concat")
(<- (concat () ?l ?l))
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))

(test* "?- (concat () (3) (3))" ""; '((?l149 3))
	   (?= (concat () (3) (3)))
	   gequal?)
(test* "?- (concat (1) (2) (1 2))" "";'(must not be nil)
	   (?= (concat (1) (2) (1 2)))
	   gequal?)
; (x . a) + b : (x . c), x=1, a=(), b=(2),c=(2)
; a + b = c
#;(test* "?- (concat (x . a) b (x . c))" '(must not be nil)
	   (?= (concat (x . a) b (x . c)))
	   gequal?)
#;(test* "?- (concat (1 2) (3 4) (1 2 3 4))" '(must not be nil)
	   (?= (concat (1 2) (3 4) (1 2 3 4)))
	   gequal?)
#|
(test-section "prolog on prolog")
(<- (prove ?goal) (prove-all (?goal)))

(<- (prove-all ()))
(<- (prove-all (?goal . ?goals))
	(clause (<- ?goal . ?body))
	(concat ?body ?goals ?new-goals)
	(prove-all ?new-goals))

(<- (clause (<- (mem ?x (?x . ?y)))))
(<- (clause (<- (mem ?x (? . ?z)) (mem ?x ?z))))

;;
(test* "?- (prove (mem ?x (1 2 3)))"
	   '(must not be nil)
	   (?= (prove (mem ?x (1 2 3))))
	   gequal?)
|#

(test-end)
