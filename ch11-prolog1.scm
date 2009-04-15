;;;; File prolog1.lisp: First version of the prolog interpreter (11.2).

(require "./auxfns")
(require "./ch11-unify")

;; Clauses are represented as (head . body) cons cells
(define (clause-head clause) (car clause))
(define (clause-body clause) (cdr clause))

;; Clauses are stored on the predicate's plistg
(define (get-clauses pred) (scheme:thru (%get pred 'clauses)))
(define (predicate relation) (car relation))

(define *db-predicates* '()); "A list of all predicates storked in the database."

(define-macro (<- . clause)
  "Add a clause to the data base."
  `(add-clause ',clause))

(define (add-clause clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let1 pred (predicate (clause-head clause))
    ;(assert (and (symbol? pred) (not (variable? pred))))
    (cl:push pred *db-predicates*) ;pushnew
    (%set! pred 'clauses
		   (cl:nconc (get-clauses pred) (list clause)))
    pred))

(define (clear-db)
  "Remove all clauses (for all predicates) from the data base."
  (cl:mapc clear-predicate *db-predicates*))

(define (clear-predicate predicate)
  "Remove the clauses for a single predicate."
  (%set! predicate 'clauses cl:nil))

(define (prove goal bindings)
  "Return a list of possible solutions to goal."  
  (mapcan (lambda (clause)
              (let1 new-clause (rename-variables clause)
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(define (prove-all goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond [(eq? bindings fail) fail]
        [(null? goals) (list bindings)]
        [else (mapcan (lambda (goal1-solution)
						(prove-all (cdr goals) goal1-solution))
					  (prove (car goals) bindings))]))

(define (rename-variables x)
  "Replace all variables in x with new ones."
  (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
				  (variables-in x))
			 x))

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

(define (find-anywhere-if predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (cl:atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (car tree))
          (find-anywhere-if predicate (cdr tree)))))

(define-macro (?- . goals) `(top-level-prove ',goals))

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

(define (variables-in exp)
  "Return a list of all the variables in EXP."
  (scheme:thru (unique-find-anywhere-if variable? exp)))

