(use srfi-1)
(require "./cl-emu")
(require "./debug")

(define-class <op> () ; An operation
  ((action   :init-keyword :action   :init-value '())
   (preconds :init-keyword :preconds :init-value '())
   (add-list :init-keyword :add-list :init-value '())
   (del-list :init-keyword :del-list :init-value '()) ))

(define op-action (cut slot-ref <> 'action))
(define op-preconds (cut slot-ref <> 'preconds))
(define op-add-list (cut slot-ref <> 'add-list))
(define op-del-list (cut slot-ref <> 'del-list))

;; p126
(define (executing? x)
  ;; Is x of the form: (executing ...) ?
  (starts-with x 'executing))

(define (starts-with lis x)
  ;; Is this a list whose first element is x?
  (and (pair? lis) (eqv? (car lis) x)))

(define (convert-op op)
  ;; Make op conform to the (EXECUTING op) convention.
  (unless (any executing? (op-add-list op))
    (slot-set! op 'add-list
               (cons (list 'executing (op-action op))
                     (op-add-list op)))
    ;(push! (op-add-list op) (list 'executing (op-action op))
    ;(print (list 'executing (op-action op)))
    )
  op)

;; p.127
(define *ops* '())
#|
(define (GPS state goals . options)
  (unless (null? options) (set! *ops* (car options)))
  (remove cl:atom (achieve-all (cons '(start) state) goals cl:nil)))
(use gauche.test)
(test* "" #f (cl:and '() '()))
(test* "" #f (cl:and '() #f))
(test* "" #f (cl:and '() #t))
(test* "" #f (cl:and #f #t))
(test* "" #t (cl:and #t #t))
|#

;; p.128
(define (achieve-all state goals goal-stack)
  ;; Try to achieve each goal, then make sure they still hold.
  (let1 current-state state
    (if (cl:and (cl:every (lambda (g)
                            (set! current-state
                                  (achieve current-state g goal-stack)))
                          goals)
                (lset<= equal? goals (or current-state '())));; (subsetp goals current-state :test #'equal)
        current-state
        #f
        )))

(define member-equal member)
(define (achieve state goal goal-stack)
  ;; A goal is achieved if it already holds,
  ;; or if there is an appropriate op for it that is applicable.
  #;(format #t " - (achieve state:.. goal:~a goal-stack:~a)\n"  goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond [(member-equal goal state) state]
        [(member-equal goal goal-stack) #f]
        [else (any (cut apply-op state goal <> goal-stack)
                   (filter (cut appropriate? goal <>) *ops*))]))

;; p.129
#;(define (member-equal item lis) (member item lis)); equal?))

(define (apply-op state goal op goal-stack)
  ;; Print a message and update state if op is applicable.
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let1 state2 (achieve-all state (op-preconds op)
                            (cons goal goal-stack))
    (if state2
        (begin
          (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
          (append
           (lset-difference equal? state2 (op-del-list op))
           (op-add-list op)))
        #f)))

(define (appropriate? goal op)
  ;; An op is appropriate to a goal if it is in its add list.
  (member-equal goal (op-add-list op)))

;; p.130
(define (use-ops oplist)
  (set! *ops* (map convert-op oplist))
  (length *ops*))

(define *school-ops*
  (list
   (make <op>
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
     :action 'ask-phone-number
     :preconds '(in-communication-with-shop)
     :add-list '(know-phone-number))
   (make <op>
     :action 'give-shop-money
     :preconds '(have-money)
     :add-list '(shop-has-money)
     :del-list '(have-money))
   ))

#;(define (GPS state goals . options)
  (unless (null? options) (set! *ops* (car options)))
  (remove cl:atom (achieve-all (cons '(start) state) goals cl:nil)))

(define (GPS state goals . options)
  (let1 old-ops *ops*
   (unless (null? options) (set! *ops* (car options)))
    (let1 r0 (or (achieve-all (cons '(start) state) goals cl:nil)
                 '())
      (let1 result (filter pair? r0)
        (set! *ops* old-ops)
        (print result)
        result))))

;(debug :gps)
(use-ops *school-ops*)
#;(GPS '(son-at-home car-needs-battery have-money)
     '(son-at-school)
     *school-ops*)
#|
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
(GPS '(son-at-home car-works)
     '(son-at-school))
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(have-money son-at-school))
(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school have-money))
(GPS '(son-at-home car-needs-battery have-money)
     '(son-at-school))
(GPS '(son-at-home) '(son-at-home))

(undebug)
;(newline)
|#