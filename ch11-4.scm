(require "./ch11-prolog")

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
	(iright ?left ?right ?rest))

(<- (= ?x ?x))

;;

(<- (zebra ?h ?w ?z)
	;; Each house is of the form:
	;; (house nationality pet cigarette drink house-color)
	(= ?h ((house norwegian ? ? ? ?)                 ;1.10
		   ?
		   (house ? ? ? milk ?) ? ?))                  ; 9
	(member (house englishman ? ? ? red) ?h)           ; 2
	(member (house spaniard dog ? ? ?) ?h)             ; 3
	(member (house ? ? ? coffee green) ?h)             ; 4
	(member (house ukrainian ? ? tea ?) ?h)            ; 5
	(iright (house ? ? ? ? ivory)                      ; 6
			(house ? ? ? ? green) ?h)
	(member (house ? snails winston ? ?) ?h)           ; 7
	(member (house ? ? kools ? yellow) ?h)             ; 8
	(nextto (house ? ? chesterfield ? ?)               ;11
			(house ? fox ? ? ?) ?h)
	(nextto (house ? ? kools ? ?)                      ;12
			(house ? horse ? ? ?) ?h)
	(member (house ? ? luckystrike orange-juice ?) ?h) ;13
	(member (house japanese ? parliaments ? ?) ?h)     ;14
	(nextto (house norwegian ? ? ? ?)                  ;15
			(house ? ? ? ? blue) ?h)
	;; Now for the questions:
	(member (house ?w ? ? water ?) ?h)                 ;Q1
	(member (house ?z zebra ? ? ?) ?h)                 ;Q2
	)

;;>>
;(?- (zebra ?houses ?water-drinker ?zebra-owner)) ;9.3sec .. 1379 LIPS

;; 11.5
(?- (length ?l 4)
	(member d ?l) (member a ?l) (member c ?l) (member b ?l)
	(= ?l (a b c d)))

;; 11.6
(define unbound "Unbound")

(define-class <var> ()
  ((name    :init-keyword :name)
   (binding :init-keyword :binding :init-value unbound)))

(define (var-name var) (slot-ref var 'name))
(define (var-binding var) (slot-ref var 'binding))

(define (bound? var) (not (eq? (var-binding var) unbound)))

(define-macro (deref exp)
  "Follow pointers for bound variables."
  `(begin (while (and (var? ,exp) (bound? ,exp))
			(set! ,exp (var-binding ,exp)))
		  ,exp))

(define (unify! x y)
  "Destructively unify two expressions."
  (cond [(eqv? (deref x) (deref y)) #t]
		[(var? x) (set-binding! x y)]
		[(var? y) (set-binding! y x)]
		[(and (pair? x) (pair? y))
		 (and (unify! (car x) (car y))
			  (unify! (cdr x) (cdr y)))]
		[else #f]))

(define (set-binding! var value)
  "Set var's binding to value.  Always succeeds (returns #t)."
;  (set! (var-binding var) value)
  (slot-set! var 'binding value))

;;
(define (print-var var stream depth)
  (if (or (and (number? *print-level*)
			   (>= depth *print-level*))
		  (var? (deref var)))
	  (format stream "?~a" (var-name var))
	  (write var stream)))

;379
(define *trail* (make-vector 200 0))

(define (set-binding! var value)
  "Set var's binding to value, after saving the variable in the trail. Always return #t."
  (unless (eq? var value)
	(vector-push-extend var *trail*)
	(slot-set! var 'binding value))
  #t)

(define (undo-bindings! old-trail)
  "Undo all bindings back to a given point in the trail."
  (until (= (fill-pointer *trail*) old-trail)
	(slot-set! (vector-pop *trail*) 'binding unbound)))

;;
(define *var-counter* 0)

(define-class <var> ()
  ((name    :init-keyword :name    :init-form (inc! *var-counter*))
   (binding :init-keyword :binding :init-form unbound)))

