;;(require "./debug") => in auxfns
(require "./prolog")
;;(require "./debug")

(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

;(<- (length #f 0))
(<- (length () 0))
(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

(<- (member ?item (?item . ?rest)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

;(debug :trace)
;(debug :assert)

;(?- (member 2 (1 2 3 2 1)))

(define (prolog-vars vars bindings other-goals)
  (with-output-to-string
	(lambda ()
	  (if (null? vars) #t
		  (dolist (var vars)
			(format #t "~a = ~a\n" var
					(subst-bindings bindings var))))
	  (prove-all other-goals bindings)
	  )))

(%set! 'prolog-vars 'clauses 'prolog-vars)

(define (top-level-prove-val goals)
  (prove-all `(,@goals (prolog-vars ,@(variables-in goals)))
			 no-bindings))

(define-macro (?= . goals)
  `(top-level-prove-val ',(replace-?-vars goals)))

;(?- (length (a b c d) ?n))
(display (?= (length (a b c d) ?n)))
