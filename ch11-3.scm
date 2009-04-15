(require "./ch11-2")

;(requires "unify") ; does not require "prolog1"

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(define (clause-head clause) (car clause))
(define (clause-body clause) (cdr clause))

;; clauses are stored on the predicate's plist
(define (get-clauses pred) (%get pred 'clauses))
(define (predicate relation) (car relation))
(define (args x) (cdr x)) ; The arguments of a relation

(define *db-predicates* '()) ; "a list of all predicates stored in the database."

#;(define-macro (<- . clause) ;; Add a clause to the data base.
  `(add-clause ',clause))
(define-macro (<- . clause)
  "add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

(define (add-clause clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let1 pred (predicate (clause-head clause))
    ;(assert (and (symbolp pred) (not (variable-p pred))))
    (cl:push pred *db-predicates*) ;; pushnew
    (%set! pred 'clauses
		   (cl:nconc (get-clauses pred) (list clause)))
    pred))

(define (clear-db)
  "remove all clauses (for all predicates) from the data base."
  (cl:mapc clear-predicate *db-predicates*))

(define (clear-predicate predicate)
  "remove the clauses for a single predicate."
  (%set! predicate 'clauses cl:nil))

(define (rename-variables x)
  "replace all variables in x with new ones."
  (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
                  (variables-in x))
			 x))

; variables-in

(define (unique-find-anywhere-if predicate tree . args)
;                                &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (let-optionals* args ((found-so-far #f))
	(if (cl:atom tree)
		(if (funcall predicate tree)
			(cl:adjoin tree found-so-far)
			found-so-far)
		(unique-find-anywhere-if
		 predicate
		 (car tree)
		 (unique-find-anywhere-if predicate (cdr tree)
								  found-so-far)))))

(define (find-anywhere-if predicate tree)
  "does predicate apply to any atom in the tree?"
  (if (cl:atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (car tree))
          (find-anywhere-if predicate (cdr tree)))))

;(defmacro ?- (&rest goals)  `(top-level-prove ',(replace-?-vars goals)))
(define-macro (?- . goals)
  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',goals))

;;;;;;;
(define (prove-all goals bindings)
  "Find a solution to the conjunction of goals."
;  (format #t "(prove-all goals:~a bindings:~a)\n" goals bindings)
  (cond [(eq? bindings fail) fail]
		[(null? goals) bindings]
		[else (prove (car goals) bindings (cdr goals))]))

#;(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (cl:some (lambda (clause)
			 (let1 new-clause (rename-variables clause)
			   (prove-all (append (clause-body new-clause) other-goals)
						  (unify goal (clause-head new-clause) bindings))))
		   (get-clauses (predicate goal))))

;; p.368
(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
;  (format #t "(prove goal:~a bindings:~a other-goals:~a)\n" goal bindings other-goals)
  (let1 clauses (get-clauses (predicate goal))
	(if (list? clauses)
		(cl:some (lambda (clause)
				   (let1 new-clause (rename-variables clause)
					 (prove-all (append (clause-body new-clause) other-goals)
								(unify goal (clause-head new-clause) bindings))))
				 clauses)
		;; The predicate's "clauses" can be an atom:
		;; a primitive function to call
		(funcall clauses (cdr goal) bindings other-goals))))

(define (top-level-prove goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
			 no-bindings)
  (format #t "\nNo.")
  (values))

(define (show-prolog-vars vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null? vars)
	  (format #t "\nYes")
	  (dolist (var vars)
		(format #t "\n~a = ~a" var
				(subst-bindings bindings var))))
  (if (continue?)
	  fail
	  (prove-all other-goals bindings)))

(%set! 'show-prolog-vars 'clauses show-prolog-vars)

(define (continue?)
  "Ask user if we should continue looking for solutions."
  (case (read-char)
	[(#\;) #t]
	[(#\.) #f]
	[(#\newline) (continue?)]
	[else (format #t " Type ; to see more or . to stop")
		  (continue?)]))

;;;;;;;;
(define (variables-in exp)
  "Return a list of all the variables in EXP."
;  (unique-find-anywhere-if non-anon-variable? exp))
  (or (unique-find-anywhere-if non-anon-variable? exp) '()))
;  (or (unique-find-anywhere-if variable? exp) '()))

(define (non-anon-variable? x)
  (and (variable? x) (not (eq? x '?))))

(define (replace-?-vars exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond [(eq? exp '?) (gensym "?")]
		[(cl:atom exp) exp]
		[else (reuse-cons (replace-?-vars (car exp))
						  (replace-?-vars (cdr exp))
						  exp)]))
