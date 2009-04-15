(require "./cl-emu")
(require "./debug")
(require "./ch04-13") ;; action?
(require "./ch04-14")
(require "./ch06-4")

(define (search-gps start goal . args)
  (let-optionals* args ((beam-width 10))
    (dbg 65 "(search-gps start:~a goal:~a beam-width:~d)" start goal beam-width)
    (filter action? (beam-search (cons '(start) start) ;; start
                                 ;;(cut cl:subsetp goal <> :test equal?)
                                 (lambda (state) (cl:subsetp goal state :test equal?)) ;; goal
                                 ;(lambda (state) (lset<= equal? goal state))
                                 gps-successors ;; successors
                                 (lambda (state) (+ (count action? state)
                                                    (count (lambda (con) (not (member con state)))
                                                           goal))) ;; cost-fn
                                 beam-width ;; beam-width
                                 ))))

(define (describe-op op)
  (format "#OP<action:~a preconds:~a add-list:~a del-list:~a>"
          (op-action op) (op-preconds op) (op-add-list op) (op-del-list op)))

(define (gps-successors state)
  (dbg 65 "(gps-successors state:~a)" state)
  (map (lambda (op)
         (append (remove (lambda (x) (member x (op-del-list op)))
                         state)
                 (op-add-list op)))
       (applicable-ops state)))

(define (applicable-ops state)
  ;; Return a list of all ops that are applicable now.
  (cl:find-all-if (lambda (op) (cl:subsetp (op-preconds op) state :test equal?)) *ops*))
