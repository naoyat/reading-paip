;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.
(require "./auxfns")
(require "./unify") ; does not require "prolog1"

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(define (clause-head clause)
  (let1 rv (car clause)
;;%	(dbg :trace "(clause-head clause:~a) => ~a" clause rv)
	rv))
(define (clause-body clause)
  (let1 rv (cdr clause)
;;%	(dbg :trace "(clause-body clause:~a) => ~a" clause rv)
	rv))

;; clauses are stored on the predicate's plist
(define (get-clauses pred)
  (trace-in 'get-clauses pred)
  (let1 rv (scheme:thru (%get pred 'clauses))
	;(dbg :trace "(get-clauses pred:~a) => ~a" pred rv)
	(trace-out 'get-clauses rv)
	rv))

(define (predicate relation)
  (let1 rv (car relation)
;;%	(dbg :trace "(predicate relation:~a) => ~a" relation rv)
	rv))
;(define (args relation) (cdr relation)) ; The arguments of a relation

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
  (trace-in 'add-clause clause)
  (let1 pred (predicate (clause-head clause))
	(dbg :assert "<ASSERT> add-clause: (symbol? pred:~a) && !(variable? pred:~a) => ~a" pred pred (and (symbol? pred) (not (variable? pred))) )
	;(format #t "  before: ~a\n" (%get pred 'clauses))
    ;(assert (and (symbolp pred) (not (variable-p pred))))
    (cl:pushnew pred *db-predicates*) ;; pushnew
;	(format #t "  c/h:~a, pred:~a, clause:~a\n" (clause-head clause) pred clause)
	(%set! pred 'clauses
		   (cl:nconc (get-clauses pred) (list clause)))
;	(format #t "   after: ~a\n" (%get pred 'clauses))
	(trace-out 'add-clause pred)
;	(dbg :trace "(add-clause clause:~a) => ~a" clause pred)
	pred))


(define (clear-db)
  "remove all clauses (for all predicates) from the data base."
  (let1 rv (cl:mapc clear-predicate *db-predicates*)
;	(dbg :trace "(clear-db) => ~a" rv)
	rv))

(define (clear-predicate predicate)
  "remove the clauses for a single predicate."
  (let1 rv (%set! predicate 'clauses cl:nil)
;	(dbg :trace "(clear-predicate predicate:~a) => ~a" predicate rv)
	rv))

(define (rename-variables x)
  "replace all variables in x with new ones."
;  (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
;                  (variables-in x))
;			 x))
  (let1 rv (cl:sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
						   (variables-in x))
					  x)
;	(dbg :trace "(rename-variables x:~a) => ~a" x rv)
	rv))

#;(define (rename-variables x)
  (dbg :trace "(rename-variables<thru> x:~a) => ~a" x x)
  x)
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
;;%	  (dbg :trace "(unique-find-anywhere-if predicate:~a tree:~a [found-so-far:~a]) => ~a"
;;%		   predicate tree found-so-far rv)
	  rv))) ;; can be '()

(define (find-anywhere-if predicate tree)
  "does predicate apply to any atom in the tree?"
  (let1 rv (if (cl:atom tree)
			   (funcall predicate tree)
			   (or (find-anywhere-if predicate (car tree))
				   (find-anywhere-if predicate (cdr tree))))
;	(dbg :trace "(find-anywhere-if predicate:~a tree:~a) => ~a" predicate tree rv)
	rv))

;(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))
;(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))
#;(define-macro (?- . goals)
  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',goals))
(define-macro (?- . goals)
;;;  (format #t "\n> ~a" (cons '?- goals))
  `(top-level-prove ',(replace-?-vars goals)))
#;(define-macro (?- . goals) `(top-level-prove ',goals))

(define (prove-all goals bindings)
  "Find a solution to the conjunction of goals."
  (trace-in 'prove-all goals bindings)
;  (dbg :trace "(PROVE-ALL goals:~a bindings:~a)" goals bindings)
  (let1 rv (cond [(eq? bindings fail) fail]
				 [(null? goals) bindings]
				 ;; [(null? goals) (list bindings)]
				 [else (prove (car goals) bindings (cdr goals))])
;	(dbg :trace "(PROVE-ALL goals:~a bindings:~a) => ~a" goals bindings rv)
;	(dbg :trace "[PROVE-ALL] => ~a" rv)
	(trace-out 'prove-all rv)
	rv))
	
;;        [else (mapcan (lambda (goal1-solution)
;;						(prove-all (cdr goals) goal1-solution))
;;					  (prove (car goals) bindings (cdr goals)))]))

;; p.368
(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
;  (dbg :trace "(PROVE goal:~a bindings:~a other-goals:~a)" goal bindings other-goals)
  (trace-in 'prove goal bindings other-goals)
  (let1 rv (let1 clauses (get-clauses (predicate goal))
			 ;(format #t "\nPROVE> goal:~a bindings:~a other-goals:~a; clauses:~a\n" goal bindings other-goals clauses)
			 (if (list? clauses)
				 ;(cl:thru
				  (cl:some (lambda (clause)
							 (let1 new-clause (rename-variables clause)
							   (prove-all (append (clause-body new-clause) other-goals)
										  (unify goal (clause-head new-clause) bindings))))
						   clauses)
				  ;)
				 ;; The predicate's "clauses" can be an atom:
				 ;; a primitive function to call
				 (funcall clauses (cdr goal) bindings other-goals)))
;	(dbg :trace "(prove goal:~a bindings:~a other-goals:~a) => ~a" goal bindings other-goals rv)
;	(dbg :trace "[PROVE] => ~a" rv)
	(trace-out 'prove rv)
	rv))

(define (top-level-prove goals)
;  (dbg :trace "(top-level-prove goals:~a)" goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
			 no-bindings)
  (format #t "No.\n")
  (values))

(define (show-prolog-vars vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null? vars)
	  (format #t "Yes\n")
	  (dolist (var vars)
		(format #t "~a = ~a\n" var
				(subst-bindings bindings var))))
  (if (continue?)
	  fail
	  (prove-all other-goals bindings)
	  )
	  )

(%set! 'show-prolog-vars 'clauses 'show-prolog-vars)

(define (continue?)
  "Ask user if we should continue looking for solutions."
  (format #t "続きを見る場合は「;」を、中断する場合は「.」を入力してください。") (flush)
;  (format #t "Type ; to see more or . to stop") (flush)
;  (case (read-char)
;	[(#\;) #t]
;	[(#\.) #f]
;	[(#\newline) (flush)];(continue?)]
;	[else (continue?)]))
  (let1 line (read-line)
	(unless (eof-object? line)
	  (case (string-ref line 0)
		[(#\;) #t]
		[(#\.) #f]
		;;[(#\newline) (flush)];(continue?)]
		[else (continue?)]
		))))
		

(define (variables-in exp)
  "Return a list of all the variables in EXP."
;  (unique-find-anywhere-if non-anon-variable? exp))
;  (scheme:thru (unique-find-anywhere-if variable? exp)))
  (let1 rv (scheme:thru
			(unique-find-anywhere-if non-anon-variable? exp))
;	(dbg :trace "(variables-in exp:~a) => ~a" exp rv)
	rv))
;  (or (unique-find-anywhere-if variable? exp) '()))

(define (non-anon-variable? x)
  (let1 rv (and (variable? x) (not (eq? x '?)))
;;%	(dbg :trace "(non-anon-variable? x:~a) => ~a" x rv)
	rv))

(define (replace-?-vars exp)
  "Replace any ? within exp with a var of the form ?123."
;  (format #t "(replace-?-vars exp:~a) =>\n" exp)
  (cond [(eq? exp '?) (gensym "?")]
		   [(cl:atom exp) exp]
		   [else (reuse-cons (replace-?-vars (car exp))
							 (replace-?-vars (cdr exp))
							 exp)]))

;;;
;(define (1+ x) (+ 1 x))

;;
;; ?=
;;
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
