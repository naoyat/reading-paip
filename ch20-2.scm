(require "./prolog")

;; Definite Clause Grammars
#|
rule (S) --> (NP) (VP)
を
(<- (S ?s0 ?s2)
	(NP ?s0 ?s1)
	(VP ?s1 ?s2))
に展開したい。
|#

(define-syntax rule
  (syntax-rules ()
	[(_ head --> . body)
	 `(funcall (%get '|-->| 'rule-function) ',head ',body) ]))

(%set! ':- 'rule-function (lambda (head body) `(<- ',head . ',body)))

;; s(Sem) --> np(Subj), vp(Pred), {combine(Subj,Pred,Sem)}.

(rule (S ?sem) --> (NP ?subj) (VP ?pred)
	  (:test (combine ?subj ?pred ?sem)))

;; verb --> [sleeps].

(rule (NP (the male) 3sg) --> (:word he))
(rule (VP sleeps 3sg) --> (:word sleeps))

(define (dcg-normal-goal? x)
  (or (starts-with x :test) (eq? x '!)))
(define (dcg-word-list? x)
  (starts-with x ':word))

(%set! '--> 'rule-function 'make-dcg)

(define (make-dcg head body)
  (let1 n (count-if (complement dcg-normal-goal? body))
	`(<- (,@head ?s0 ,(symbol '?s n))
		 .,(make-dcg-body body 0))))
  
(define (make-dcg-body body n)
  (if (null? body)
	  '()
	  (let1 goal (car body)
		(cond
		 [(eq? goal '!)
		  (cons '! (make-dcg-body (cdr body) n))]
		 [(dcg-normal-goal? goal)
		  (append (cdr goal)
				  (make-dcg-body (cdr body) n))]
		 [(dcg-word-list? goal)
		  (cons `(= ,(symbol '?s n)
					(,@(cdr goal) . ,(symbol '?s (+ n 1))))
				(make-dcg-body (cdr body) (+ n 1)))]
		 [else
		  (cons (append goal (list (symbol '?s n)
								   (symbol '?s (+ n 1))))
				(make-dcg-body (cdr body) (+ n 1)))]
		 ))))