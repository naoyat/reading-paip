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

(define (symbol . exprs)
  (string->symbol (apply string-append (map x->string exprs))))

#;(define-syntax rule_
  (syntax-rules ()
	[(_ head arrow . body)
	 `(funcall (%get ',arrow 'rule-function) ,head ,body) ]
	[(_ head --> . body)
	 `(funcall (%get '|-->| 'rule-function) ',head ',body) ]))

(define-macro (rule head . args)
  (let-optionals* args ((arrow '|:-|) . body)
	(let1 rv (funcall (%get arrow 'rule-function) head body)
	  (print rv)
	  rv)))

(%set! '|:-| 'rule-function (lambda (head body) `(<- ,head . ,body)))

;; s(Sem) --> np(Subj), vp(Pred), {combine(Subj,Pred,Sem)}.

(define (dcg-normal-goal? x)
  (or (starts-with x :test) (eq? x '!)))
(define (dcg-word-list? x)
  (starts-with x :word))

(define (make-dcg head body)
  (let1 n (count (complement dcg-normal-goal?) body)
	`(<- (,@head ?s0 ,(symbol '?s n))
		 . ,(make-dcg-body body 0))))

(%set! '|-->| 'rule-function make-dcg)

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

#|
(use file.util)
(define (load-rules file)
  ;(let1 rule-func (%get '|-->| 'rule-function)
  (for-each (lambda (sexp)
										;(print "> " sexp)
			  (case (car sexp)
				[(rule)
				 (let* ([head (second sexp)]
						[arrow (third sexp)]
						[arrow-func (%get '|-->| 'rule-function)]
						[body (cdddr sexp)])
				   (print "(<- " sexp ")")
				   (funcall arrow-func head body)
				   )]
				[else
				 (print "(<- " sexp ")")
				 (add-clause (replace-?-vars (list sexp))) ; <- sexp
				 ]
				))
			(file->sexp-list file)))
|#
