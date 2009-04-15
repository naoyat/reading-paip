(require "./ch04-12")
;;; 4.13
(define (make-maze-ops pair)
  (list (make-maze-op (first pair) (second pair))
		(make-maze-op (second pair) (first pair))))

(define (make-maze-op here there)
  (make <op>
	:action `(move from ,here to ,there)
	:preconds `((at ,here))
	:add-list `((at ,there))
	:del-list `((at ,here))))
  
#;(define (mappend fn the-list)
  (apply append (map fn the-list)))

(define *maze-ops*
  (append-map make-maze-ops
			  '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
				(12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
				(23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))
#|
(use-ops *maze-ops*)
(GPS '((at 1)) '((at 25)))

(use-ops *maze-ops*)
(debug :gps)
(GPS '((at 25)) '((at 1)))
;(GPS '((at 1)) '((at 25)))
|#
(define (action? x)
  (or (equal? x '(start)) (executing? x)))

(define (GPS state goals . options)
  (let1 old-ops *ops*
    (unless (null? options) (set! *ops* (car options)))
	(let1 r0 (or (achieve-all (cons '(start) state) goals cl:nil)
				 '())
	  #;(dbg :gps "")
	  (filter action? r0))))

(define (find-path start end)
  (let1 results (GPS `((at ,start)) `((at ,end)))
	(unless (null? results)
	  (cons start (map destination (remove (cut equal? '(start) <>) results))))))

(define (destination action)
  (fifth (second action)))

(use-ops *maze-ops*)
#|
#?=(find-path 1 25)
#?=(find-path 25 1)
#?=(find-path 20 25)
#?=(find-path 25 20)
#?=(find-path 25 25)
(find-path 1 1)
#?=(equal? (find-path 1 25) (reverse (find-path 25 1)))
|#