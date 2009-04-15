(require "./patmatch")

(define *occurs-check* #t); "Should we do the occurs check?"

(define (unify x y . args)
  "See if x and y match with given bindings."
  (let-optionals* args ((bindings no-bindings))
	(trace-in 'unify x y bindings)
	(let1 rv (cond [(eq? bindings fail) fail]
				   [(eqv? x y) bindings]
				   [(variable? x) (unify-variable x y bindings)]
				   [(variable? y) (unify-variable y x bindings)]
				   [(and (pair? x) (pair? y))
					(unify (cdr x) (cdr y)
						   (unify (car x) (car y) bindings))]
				   [else fail])
;	  (dbg :trace "(unify x:~a y:~a [bindings:~a]) => ~a" x y bindings rv)
	  (trace-out 'unify rv)
	  rv)))

(define (unify-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (trace-in 'unify-variable var x bindings)
  (let1 rv (cond [(get-binding var bindings)
				  (unify (lookup var bindings) x bindings)]
				 [(and (variable? x) (get-binding x bindings))
				  (unify var (lookup x bindings) bindings)]
				 [(and *occurs-check* (occurs-check var x bindings))
				  fail]
				 [else (extend-bindings var x bindings)])
;	(dbg :trace "(unify-variable var:~a x:~a bindings:~a) => ~a" var x bindings rv)
	(trace-out 'unify-variable rv)
	rv))

(define (occurs-check var x bindings)
  "Does var occur anywhere inside x?"
  (trace-in 'occurs-check var x bindings)
;  (dbg :trace "(OCCURS-CHECK var:~a x:~a bindings:~a)" var x bindings)
  (let1 rv (cond [(eq? var x) #t]
				 [(and (variable? x) (get-binding x bindings))
				  (occurs-check var (lookup x bindings) bindings)]
				 [(pair? x) (or (occurs-check var (car x) bindings)
								(occurs-check var (cdr x) bindings))]
				 [else #f])
;	(dbg :trace "(occurs-check does {~a} occurs in {~a}; bindings:~a) => ~a" var x bindings rv)
;	(dbg :trace "[OCCURS-CHECK] => ~a" rv)
	(trace-out 'occurs-check rv)
	rv))

;;; ==============================

(define (subst-bindings bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (trace-in 'subst-bindings bindings x)
  (let1 rv
	  (cond [(eq? bindings fail) fail]
			[(eq? bindings no-bindings) x]
			[(and (variable? x) (get-binding x bindings))
			 (subst-bindings bindings (lookup x bindings))]
			[(cl:atom x) x]
			[else (reuse-cons (subst-bindings bindings (car x))
							  (subst-bindings bindings (cdr x))
							  x)])
;	(dbg :trace "(subst-binding bindings:~a x:~a) => ~a" bindings x rv)
	(trace-out 'subst-bindings rv)
	rv))

;;; ==============================

(define (unifier x y)
 "Return something that unifies with both x and y (or fail)."
 (let1 rv (subst-bindings (unify x y) x)
;   (dbg :trace "(unifier x:~a y:~a) => ~a" x y rv)
   rv))

