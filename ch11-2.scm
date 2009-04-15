(require "./cl-emu")

;; p.333 (ch10-4) avoid unnecessary consing
(define (reuse-cons x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eqv? x (car x-y)) (eqv? y (cdr x-y)))
	  x-y
	  (cons x y)))

;(require "./ch06-2")
;; 06-2と同じ
(define (pat-match pattern input . args)
  (let-optionals* args ((bindings no-bindings))
;	(format #t "pattern:~a input:~a bindings:~a\n" pattern input bindings)
    (cond [(eq? bindings fail) fail]
          [(variable? pattern)
           (match-variable pattern input bindings)]
          [(eqv? pattern input) bindings]
;          [(segment-pattern? pattern)
;           (segment-matcher pattern input bindings)]
;          [(single-pattern? pattern)
;           (single-matcher pattern input bindings)]
          [(and (pair? pattern) (pair? input))
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input)
                                 bindings))]
          [else fail])))

(define fail cl:nil) ; Indicates pat-match failure

(define no-bindings '((#t . #t))) ; Indicates pat-match success, with no variables.

(define (variable? x)
  "Is x a variable (a symbol beginning with '?')?"
;;(and (symbol? x) (equal? (char (symbol-name x) 0) #\?)))
  (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?)))

(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
  (assoc var bindings))

(define (binding-val binding)
  ;; Get the value part of a single binding
  (cl:thru (cl:cdr binding)))

(define (make-binding var val) (cons var val)) ;;;

(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (binding-val (get-binding var bindings)))

(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (cons (make-binding var val)
		;; Once we add a "real" binding,
		;; we can get rid of the dummy no-bindings
        (if (eq? bindings no-bindings)
            '()
            bindings)))

(define (match-variable var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let1 binding (get-binding var bindings)
;	(format #t "(match-variable: input:~a, var:~a, binding:~a)\n" input var binding)
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

(define %ht (make-hash-table 'equal?))
(define (%get sym key)
  (hash-table-get %ht (cons sym key) #f));;(if #f #f)))
(define (%set! sym key val)
  (hash-table-put! %ht (cons sym key) val)
  val)
(define (funcall fn . args)
  (let1 proc (cond [(procedure? fn) fn]
                   [(symbol? fn)
                    (global-variable-ref (current-module) fn)]
                   [else #f])
    (when fn (apply proc args))))

;...

;;;
;;; unify - buggy version
(define (unify0 x y . args)
  "See if x and y match with given bindings."
  (let-optionals* args ((bindings no-bindings))
;	(format #t "pattern:~a input:~a bindings:~a\n" pattern input bindings)
    (cond [(eq? bindings fail) fail]
          [(variable? x) (unify0-variable x y bindings)]
          [(variable? y) (unify0-variable y x bindings)]
          [(eqv? x y) bindings]
          [(and (pair? x) (pair? y))
		   (unify0 (cdr x) (cdr y)
				  (unify0 (car x) (car y) bindings))]
		  [else fail])))

(define (unify0-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
	  (unify0 (lookup var bindings) x bindings)
	  (extend-bindings var x bindings)))

;;; better one
(define (unify1 x y . args)
  "See if x and y match with given bindings."
  (let-optionals* args ((bindings no-bindings))
;	(format #t "pattern:~a input:~a bindings:~a\n" pattern input bindings)
    (cond [(eq? bindings fail) fail]
          [(eqv? x y) bindings] ;;
          [(variable? x) (unify1-variable x y bindings)]
          [(variable? y) (unify1-variable y x bindings)]
          [(and (pair? x) (pair? y))
		   (unify1 (cdr x) (cdr y)
				  (unify1 (car x) (car y) bindings))]
		  [else fail])))

(define (unify1-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  ;; Warning - buggy version
  (if (get-binding var bindings)
	  (unify1 (lookup var bindings) x bindings)
	  (extend-bindings var x bindings)))


;;;
(define (unify2 x y . args)
  "See if x and y match with given bindings."
  (let-optionals* args ((bindings no-bindings))
;	(format #t "pattern:~a input:~a bindings:~a\n" pattern input bindings)
    (cond [(eq? bindings fail) fail]
          [(eqv? x y) bindings] ;;
          [(variable? x) (unify2-variable x y bindings)]
          [(variable? y) (unify2-variable y x bindings)]
          [(and (pair? x) (pair? y))
		   (unify2 (cdr x) (cdr y)
				  (unify2 (car x) (car y) bindings))]
		  [else fail])))

(define (unify2-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond [(get-binding var bindings)
		 (unify2 (lookup var bindings) x bindings)]
		[(and (variable? x) (get-binding x bindings))
		 (unify2 var (lookup x bindings) bindings)]
		[else
		 (extend-bindings var x bindings)]))

;;;
(define *occurs-check* #t) ; should we do the occurs check?

(define (unify x y . args) ; = unify2
  "See if x and y match with given bindings."
  (let-optionals* args ((bindings no-bindings))
;	(format #t "(unify x:~a y:~a bindings:~a)\n" x y bindings)
    (cond [(eq? bindings fail) fail]
          [(eqv? x y) bindings] ;;
          [(variable? x) (unify-variable x y bindings)]
          [(variable? y) (unify-variable y x bindings)]
          [(and (pair? x) (pair? y))
		   (unify (cdr x) (cdr y)
				  (unify (car x) (car y) bindings))]
		  [else fail])))

(define (unify-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
;  (format #t "(unify-variable var:~a x:~a bindings:~a)\n" var x bindings)
  (cond [(get-binding var bindings)
		 (unify (lookup var bindings) x bindings)]
		[(and (variable? x) (get-binding x bindings))
		 (unify var (lookup x bindings) bindings)]
		[(and *occurs-check* (occurs-check var x bindings))
		 fail]
		[else
		 (extend-bindings var x bindings)]))

(define (occurs-check var x bindings)
  "Does var occur anywhere inside x?"
;  (format #t "occurs-check var:~a x:~a in bindings:~a..\n" var x bindings)
  (cond [(eq? var x) #t]
		[(and (variable? x) (get-binding x bindings))
		 (occurs-check var (lookup x bindings) bindings)]
		[(pair? x) (or (occurs-check var (car x) bindings)
					   (occurs-check var (cdr x) bindings))]
		[else #f]))

;;; p.357
(define (subst-bindings bindings x)
  "Substitute the value of variables in bindings into x,
   taking recursively bound variables into account."
  (cond [(eq? bindings fail) fail]
		[(eq? bindings no-bindings) x]
		[(and (variable? x) (get-binding x bindings))
		 (subst-bindings bindings (lookup x bindings))]
		[(cl:atom x) x]
		[else (reuse-cons (subst-bindings bindings (car x))
						  (subst-bindings bindings (cdr x))
						  x)]))

(define (unifier x y)
  "Return something that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))



;; p.360

;; Clauses are represented as (head . body) cons cells
(define (clause-head clause) (car clause))
(define (clause-body clause) (cdr clause))

;; Clauses are stored on the predicate's plist
(define (get-clauses pred) (or (%get pred 'clauses) cl:nil))
(define (predicate relation) (car relation))

(define *db-predicates* '()); A list of all predicates stored in the database.

(define-macro (<- . clause) ;; Add a clause to the data base.
;  (print "<- " clause)
  `(add-clause ',clause))

(define (add-clause clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let1 pred (predicate (clause-head clause))
	;(assert (and (symbol? pred) (not (variable? pred))))
	;;(pushnew pred *db-predicates*)
	(cl:push pred *db-predicates*)
	(%set! pred 'clauses (cl:nconc (get-clauses pred) (list clause)))
	pred))

;p.362
(define (clear-db)
  "Remove all clauses (for all predicates) from the data base."
  (cl:mapc clear-predicate *db-predicates*))

(define (clear-predicate predicate)
  "Remove the clauses for a single predicate."
  (%set! predicate 'clauses '()))

(define (prove goal bindings)
  "Return a list of possible solutions to goal."
;  (format #t "(prove goal:~a bindings:~a)\n" goal bindings)
  (cl:mapcan (lambda (clause)
			   (let1 new-clause (rename-variables clause)
					 (prove-all (clause-body new-clause)
							(unify goal (clause-head new-clause) bindings))))
			 (get-clauses (predicate goal))))

(define (prove-all goals bindings)
  "Return a list of solutions to the conjunction of goals."
;  (format #t "\n(prove-all goals:~a bindings:~a)\n" goals bindings)
  (cond [(eq? bindings fail) fail]
		[(null? goals) (list bindings)]
		;[(and (print 2) #f)]
		[else (cl:mapcan (lambda (goal1-solution)
						   (prove-all (cdr goals) goal1-solution))
						 (prove (car goals) bindings))]))

;p.363
(define (rename-variables x)
  "Replace all variables in x with new ones."
;  (format #t "(rename-variables x:~a)\n" x)
  (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
				  (variables-in x))
			 x))

(define (variables-in exp)
  "Return a list of all the variables in EXP."
  (or (unique-find-anywhere-if variable? exp) '()))

(define (unique-find-anywhere-if predicate tree . args)
  "Return a list of leaves of tree satisfying predicate,
   with duplicates removed."
  (let-optionals* args ((found-so-far #f))
	(if (cl:atom tree)
		(if (funcall predicate tree)
			(cl:adjoin tree found-so-far)
			found-so-far)
		(unique-find-anywhere-if predicate
								 (car tree)
								 (unique-find-anywhere-if predicate (cdr tree)
														  found-so-far)))))

(define-macro (?- . goals)
  `(prove-all ',goals no-bindings))

;; p.364
(define-macro (?- . goals)
  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',goals))

(define (top-level-prove goals)
  "Prove the goals, and print variables readably."
  (show-prolog-solutions (variables-in goals)
						 (prove-all goals no-bindings)))

(define (show-prolog-solutions vars solutions)
  "Print the variables in each of the solutions."
  (if (null? solutions)
	  (format #t "\nNo.")
	  (cl:mapc (lambda (solution) (show-prolog-vars vars solution))
			   solutions))
  (newline)
  (values))

(define (show-prolog-vars vars bindings)
  "Print each variable with its binding."
  (if (null? vars)
	  (format #t "\nYes.")
	  (dolist (var vars)
		(format #t "\n~a = ~a" var 
				(subst-bindings bindings var))))
  (cl:princ ";"))
