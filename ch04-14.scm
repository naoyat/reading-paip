(require "./ch04-13")
(use srfi-42)

(define (make-block-ops blocks)
  (let1 ops '()
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal? a b)
          (dolist (c blocks)
            (unless (or (equal? c a) (equal? c b))
              (cl:push (move-op a b c) ops)))
          #;(format #t "[a:~a b:~a] ~a\n" a b ops)
          (cl:push (move-op a 'table b) ops)
          (cl:push (move-op a b 'table) ops))))
    ops))

(define (move-op a b c)
  (make <op>
    :action `(move ,a from ,b to ,c)
    :preconds `((space on ,a) (space on ,c) (,a on ,b))
    :add-list (move-ons a b c)
    :del-list (move-ons a c b)))

(define (move-ons a b c)
  (if (eq? b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

#|
;(debug :gps)
(use-ops (make-block-ops '(a b)))

#;(map print
 (GPS '((a on table) (b on table) (space on a) (space on b)
        (space on table))
      '((a on b) (b on table)))
 )

#;(map print
 (GPS '((a on b) (b on table) (space on a) (space on table))
      '((b on a)))
 )
;(undebug)

(use-ops (make-block-ops '(a b c)))
#;(map print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
      '((b on a) (c on b)))
 )
;(debug :gps)
#;(map print
	 #?=(GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
			 '((c on b) (b on a)))
	 )
;;
|#
(define (achieve-all state goals goal-stack)
  (any (lambda (goals) (achieve-each state goals goal-stack))
       (orderings goals)))

(define (achieve-each state goals goal-stack)
  (let1 current-state state
     (if (cl:and (cl:every (lambda (g)
                             (set! current-state
                                   (achieve current-state g goal-stack)))
                           goals)
                 (lset<= equal? goals (or current-state '())))
         ;(subsetp goals current-state 'equal))
         current-state
         #f)))

(define (orderings l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

#|
(map print
     (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
             '((c on b) (b on a)))
     )

(map print
     (GPS '((c on a) (a on table) (b on table)
            (space on c) (space on b) (space on table))
          '((c on table)))
     )

(map print
     (GPS '((c on a) (a on table) (b on table)
            (space on c) (space on b) (space on table))
          '((c on table) (a on b)))
     )
|#

(define (achieve state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond [(member-equal goal state) state]
        [(member-equal goal goal-stack) #f]
        [else (any (lambda (op) (apply-op state goal op goal-stack))
                   (appropriate-ops goal state))]))
(define (appropriate-ops goal state)
  (define (fn op) (count (lambda (precond)
                           (not (member-equal precond state)))
                         (op-preconds op)))
  (sort (list-copy (filter (cut appropriate? goal <>) *ops*))
        (lambda (x y) (< (fn x) (fn y))) ))

#|
(map print
     (GPS '((c on a) (a on table) (b on table)
            (space on c) (space on b) (space on table))
          '((c on table) (a on b)))
     )

(map print
	 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
          '((b on a) (c on b)))
     )

(map print
     (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
          '((c on b) (b on a)))
     )


(define start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table)))
(print (GPS start '((a on b) (b on c))))
(print (GPS start '((b on c) (a on b))))
|#
