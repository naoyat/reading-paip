(require "./cl-emu")
(require "./debug")
(use srfi-1)
(use slib)
(require 'trace)

;(define (subsetp list1 list2) (lset<= eq? list1 list2))
;(define *state* '())  ; The Current state: a list of conditions.
;(define *ops* '())  ; A list of available operators.

(define-class <op> () ; An operation
  ((action   :init-keyword :action   :init-value '())
   (preconds :init-keyword :preconds :init-value '())
   (add-list :init-keyword :add-list :init-value '())
   (del-list :init-keyword :del-list :init-value '()) ))

(define (GPS state goals ops)
  ;; General Problem Solver: achieve all goals using ops.
  (define (achieve goal)
    ;; A goal is achieved if it already holds,
    ;; or if there is an appropriate op for it that is applicable.
	#;(format #t "[achieve] goal:~a state:~a // ~a || ~a => ~a\n" goal state
			(member goal state)
			(map (cut slot-ref <> 'action) (filter (cut appropriate? goal <>) ops))
			(any apply-op (filter (cut appropriate? goal <>) ops))
			)
	(dbg :gps "The current goal is ~a" goal)
    (or (member goal state)
        (any apply-op (filter (cut appropriate? goal <>) ops))))

  (define (appropriate? goal op)
    ;; An op is appropriate to a goal if it is in its add list.
	#;(format #t "[appropriate?] goal:~a op:~a // ~a\n"
			goal
			(slot-ref op 'action)
			(member goal (slot-ref op 'add-list))
			)
    (member goal (slot-ref op 'add-list)))

  (define (achieve-all goals)
	;; Try to achieve each goal, then make sure they still hold.
	(and (every achieve goals) (subsetp goals state)))

  (define (apply-op op)
    ;; Print a message and update state if op is applicable.
	#;(format #t "[apply-op] op:~a => ~a\n"
			(slot-ref op 'action)
			;(slot-ref op 'preconds)
			(every achieve (slot-ref op 'preconds)) )
    (if (achieve-all (slot-ref op 'preconds));(every achieve (slot-ref op 'preconds))
		(begin
		  (print (list 'executing (slot-ref op 'action)))
		  #;(format #t "  state:~a" state)
		  (set! state (lset-difference eq? state (slot-ref op 'del-list)))
		  (set! state (lset-union eq? state (slot-ref op 'add-list)))
		  #;(format #t " => ~a\n" state)
		  #t)
		#f))

  ;;(trace achieve)
  (print "================")
  (if (achieve-all goals);;(every achieve goals)
	  (begin (print "solved.")
			 'solved)
	  (begin (print "NIL")
			 #f)))

(define *school-ops*
  (list (make <op>
          :action 'drive-son-to-school
          :preconds '(son-at-home car-works)
          :add-list '(son-at-school)
          :del-list '(son-at-home))
        (make <op>
          :action 'shop-installs-battery
          :preconds '(car-needs-battery shop-knows-problem shop-has-money)
          :add-list '(car-works))
        (make <op>
          :action 'tell-shop-problem
          :preconds '(in-communication-with-shop)
          :add-list '(shop-knows-problem))
        (make <op>
          :action 'telephone-shop
          :preconds '(know-phone-number)
          :add-list '(in-communication-with-shop))
        (make <op>
          :action 'look-up-number
          :preconds '(have-phone-book)
          :add-list '(know-phone-number))
        (make <op>
          :action 'give-shop-money
          :preconds '(have-money)
          :add-list '(shop-has-money)
          :del-list '(have-money))
        ))
#|
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)

(GPS '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)

(GPS '(son-at-home car-works)
     '(son-at-school)
     *school-ops*)

;;;
(GPS '(son-at-home have-money car-works)
     '(have-money son-at-school)
     *school-ops*)

; must fail => 4.7
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school)
     *school-ops*)
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school have-money)
     *school-ops*)
;|#
;; 4.9
(push! *school-ops*
       (make <op>
         :action 'ask-phone-number
         :preconds '(in-communication-with-shop)
         :add-list '(know-phone-number)))

;(trace achieve)


;(debug :gps)
#|
(GPS '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)
|#