(require "./cl-emu")

(define *titles* '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General))
(define *tail-titles* '(MD Jr.))

;;1.1
(define (last-name name)
  (let1 l (car (cl:last name))
	(if (memq l *tail-titles*)
		(car (cl:last name 2))
		l)))

;;1.2
(define (power a b)
  (let loop ((p 1) (n b))
	(if (zero? n) p (loop (* p a) (- n 1)) )))

;;1.3
(define (count-atoms l)
  (if (null? l) 0
	  (if (pair? l)
		  (apply + (map count-atoms l))
		  1)))

;;1.4
(define (count-anywhere e l)
  (define (sub l)
	(if (null? l) 0
		(if (pair? l)
			(apply + (map sub l))
			(if (eq? e l) 1 0))))
  (sub l))

;;1.5
(define (dot-product la lb) (apply + (map * la lb)))

;;
(use gauche.test)
(test-start "1.11 Exercises")
(test-section "問題1.1")
(test* "Rex Morgan MD" 'Morgan (last-name '(Rex Morgan MD)))
(test* "Morton Downey, Jr." 'Downey (last-name '(Morton Downey Jr.)))
(test-section "問題1.2")
(test* "3^2" 9 (power 3 2))
(test* "3^1" 3 (power 3 1))
(test* "3^0" 1 (power 3 0))
(test-section "問題1.3")
(test* "(a (b) c)" 3 (count-atoms '(a (b) c))) ;;
(test* "(a () c)" 2 (count-atoms '(a () c))) ;; schemeだとnilって無いから
(test-section "問題1.4")
(test* "" 3 (count-anywhere 'a '(a ((a) b) a)))
(test* "" 1 (count-anywhere 'b '(a ((a) b) a)))
(test* "" 0 (count-anywhere 'c '(a ((a) b) a)))
(test* "" 1 (count-anywhere 'a 'a))
(test* "" 1 (count-anywhere 'a '(a)))
(test* "" 0 (count-anywhere 'a '()))
(test-section "問題1.5")
(test* "" 110 (dot-product '(10 20) '(3 4)))
(test-end)

