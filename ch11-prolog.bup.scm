;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.
#|
(define fail '())
(define no-bindings '((#t . #t)))
(define (reuse-cons x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eqv? x (car x-y)) (eqv? y (cdr x-y)))
	  x-y
	  (cons x y)))
(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
  (assoc var bindings))
(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (cons (make-binding var val)
		;; Once we add a "real" binding,
		;; we can get rid of the dummy no-bindings
        (if (eq? bindings no-bindings)
            '()
            bindings)))
(define (make-binding var val) (cons var val))
(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (format #t "Lookup var:~a ... => ~a\n" var
		  (binding-val (get-binding var bindings)))
  (binding-val (get-binding var bindings)))
(define (binding-val binding)
  ;; Get the value part of a single binding
  (cl:thru (cl:cdr binding)))
|#
(require "./auxfns")
(require "./ch11-unify") ; does not require "prolog1"

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(define (clause-head clause) (car clause))
(define (clause-body clause) (cdr clause))

;; clauses are stored on the predicate's plist
(define (get-clauses pred) (scheme:thru (%get pred 'clauses)))
(define (predicate relation) (car relation))
;(define (args x) (cdr x)) ; The arguments of a relation

(define *db-predicates* '()) ; "a list of all predicates stored in the database."

#;(define-macro (<- . clause) ;; Add a clause to the data base.
 `(add-clause ',clause))
(define-macro (<- . clause)
  "add a clause to the data base."
;  `(add-clause ',clause))
  `(add-clause ',(replace-?-vars clause)))

(define (add-clause clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
;  (format #t "add-clause ~a..\n" clause)
  (let1 pred (predicate (clause-head clause))
	;(format #t "  before: ~a\n" (%get pred 'clauses))
    ;(assert (and (symbolp pred) (not (variable-p pred))))
    (cl:pushnew pred *db-predicates*) ;; pushnew
;	(format #t "  c/h:~a, pred:~a, clause:~a\n" (clause-head clause) pred clause)
	(%set! pred 'clauses
		   (cl:nconc (get-clauses pred) (list clause)))
;	(format #t "   after: ~a\n" (%get pred 'clauses))
	pred))



(define (clear-db)
  "remove all clauses (for all predicates) from the data base."
  (cl:mapc clear-predicate *db-predicates*))

(define (clear-predicate predicate)
  "remove the clauses for a single predicate."
  (%set! predicate 'clauses cl:nil))

(define (rename-variables x)
  "replace all variables in x with new ones."
;  (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
;                  (variables-in x))
;			 x))
  (let1 rv (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
						   (variables-in x))
					  x)
	(dbg :trace "(rename-variables x:~a) => ~a" x rv)
	rv))

(define (rename-variables x) x)

; variables-in

(define (unique-find-anywhere-if predicate tree . args)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (let-optionals* args ((found-so-far '()))
	(let1 rv (if (cl:atom tree)
				 (if (funcall predicate tree)
					 (cl:adjoin tree found-so-far)
					 found-so-far)
				 (unique-find-anywhere-if predicate
										  (car tree)
										  (unique-find-anywhere-if predicate (cdr tree)
																   found-so-far)))
	  (dbg :trace "(unique-find-anywhere-if predicate:~a tree:~a [found-so-far:~a]) => ~a"
		   predicate tree found-so-far rv)
	  rv)))

(define (find-anywhere-if predicate tree)
  "does predicate apply to any atom in the tree?"
  (let1 rv (if (cl:atom tree)
			   (funcall predicate tree)
			   (or (find-anywhere-if predicate (car tree))
				   (find-anywhere-if predicate (cdr tree))))
	(dbg :trace "(find-anywhere-if predicate:~a tree:~a) => ~a" predicate tree rv)
	rv))

;(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))
;(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))
#;(define-macro (?- . goals)
  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',goals))
(define-macro (?- . goals)
  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',(replace-?-vars goals)))
#;(define-macro (?- . goals) `(top-level-prove ',goals))

(define (prove-all goals bindings)
  "Find a solution to the conjunction of goals."
  (let1 rv (cond [(eq? bindings fail) fail]
				 [(null? goals) bindings]
										;       [(null? goals) (list bindings)]
				 [else (prove (car goals) bindings (cdr goals))])
	(dbg :trace "(prove-all goals:~a bindings:~a) => ~a" goals bindings rv)
	rv))
	
										;        [else (mapcan (lambda (goal1-solution)
;						(prove-all (cdr goals) goal1-solution))
;					  (prove (car goals) bindings (cdr goals)))]))
#|
(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (cl:some (lambda (clause)
			 (let1 new-clause (rename-variables clause)
			   (prove-all (append (clause-body new-clause) other-goals)
						  (unify goal (clause-head new-clause) bindings))))
		   (get-clauses (predicate goal))))
|#

;; p.368
(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let1 rv (let1 clauses (get-clauses (predicate goal))
			 ;(format #t "\nPROVE> goal:~a bindings:~a other-goals:~a; clauses:~a\n" goal bindings other-goals clauses)
			 (if (list? clauses)
				 (cl:some (lambda (clause)
							(let1 new-clause (rename-variables clause)
							  #;(format #t " # ~a == ~a; (prove-all goals:~a bindings:~a)\n" clause new-clause
							  (append (clause-body new-clause) other-goals)
							  (unify goal (clause-head new-clause) bindings))
							  ;(format #t " ??> (prove-all ~a (unify goal:~a heads:~a bindings:~a))\n"
							  ;(append (clause-body new-clause) other-goals)
							  ;goal (clause-head new-clause) bindings)
							  (prove-all (append (clause-body new-clause) other-goals)
										 (unify goal (clause-head new-clause) bindings))))
						  clauses)
		;; The predicate's "clauses" can be an atom:
		;; a primitive function to call
				 (funcall clauses (cdr goal) bindings other-goals)))
	(dbg :trace "(prove goal:~a bindings:~a other-goals:~a) => ~a" goal bindings other-goals rv)
	rv))

(define (top-level-prove goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
			 no-bindings)
  (format #t "\nNo.\n")
  (values))

(define (show-prolog-vars vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null? vars)
	  (format #t "\nYes")
	  (dolist (var vars)
		(format #t "\n~a = ~a" var
				(subst-bindings bindings var))))
;  (prove-all other-goals bindings))
  (if (continue?)
	  fail
	  (prove-all other-goals bindings)))

#|
(define (top-level-prove goals)
  "Prove the goals, and print variables readably."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals no-bindings)))
(define (show-prolog-solutions vars solutions)
  "Print the variables in each of the solutions."  
  (if (null? solutions)
      (format #t "\nNo.")
      (mapc (lambda (solution) (show-prolog-vars vars solution))
            solutions))
  (values))
(define (show-prolog-vars vars bindings)
  "Print each variable with its binding."
  (if (null? vars)
      (format #t "\nYes")
      (dolist (var vars)
        (format #t "\n~a = ~a" var
                (subst-bindings bindings var))))
  (princ ";"))
|#
(%set! 'show-prolog-vars 'clauses 'show-prolog-vars)

(define (continue?)
  "Ask user if we should continue looking for solutions."
  (case (read-char)
	[(#\;) #t]
	[(#\.) #f]
	[(#\newline) (continue?)]
	[else (format #t " Type ; to see more or . to stop")
		  (flush)
		  (continue?)]))

(define (variables-in exp)
  "Return a list of all the variables in EXP."
;  (unique-find-anywhere-if non-anon-variable? exp))
;  (scheme:thru (unique-find-anywhere-if variable? exp)))
  (scheme:thru 
   (unique-find-anywhere-if non-anon-variable? exp)))
;  (or (unique-find-anywhere-if variable? exp) '()))

(define (non-anon-variable? x)
  (and (variable? x) (not (eq? x '?))))

(define (replace-?-vars exp)
  "Replace any ? within exp with a var of the form ?123."
;  (format #t "(replace-?-vars exp:~a) =>\n" exp)
  (cond [(eq? exp '?) (gensym "?")]
		   [(cl:atom exp) exp]
		   [else (reuse-cons (replace-?-vars (car exp))
							 (replace-?-vars (cdr exp))
							 exp)]))

;;;
(define (1+ x) (+ 1 x))
