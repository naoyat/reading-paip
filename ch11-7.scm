(require "./ch11-prolog")
#|
(define (concat . sequences)
  (format #t "(concat ~a)\n" sequences)
  (cond [(symbol? (car sequences))
		 (string->symbol (apply string-append (map symbol->string sequences)))]
		[(string? (car sequences))
		 (string-append sequences)]
		[(list? (car sequences))
		 (append sequences)]))
|#
(<- (member ?item (?item . ?rest)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

;;(?- (member 2 (1 2 3 2 1)))

;(<- (length () 0))
;(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

;; Zebra puzzle
;(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))
;(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))

;(<- (iright ?left ?right (?left ?right . ?rest)))
;(<- (iright ?left ?right (?x . ?rest))
;    (iright ?left ?right ?rest))

(<- (= ?x ?x))

;;
;(<- (rev () ()))
;(<- (rev (?x . ?a) ?b) (rev ?a ?c) (concat ?c (?x) ?b))

(<- (concat () ?l ?l))
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))

(?- (concat () (3) (3)))
(?- (concat (1 2) (3 4) (1 2 3 4)))

;(<- (irev ?l ?r) (irev3 ?l () ?r))
;(<- (irev3 (?x . ?l) ?so-far ?r) (irev3 ?l (?x . ?so-far) ?r))
;(<- (irev3 () ?r ?r))

;;
(<- (prove ?goal) (prove-all (?goal)))

(<- (prove-all ()))
(<- (prove-all (?goal . ?goals))
	(clause (<- ?goal . ?body))
	(concat ?body ?goals ?new-goals)
	(prove-all ?new-goals))

(<- (clause (<- (mem ?x (?x . ?y)))))
(<- (clause (<- (mem ?x (? . ?z)) (mem ?x ?z))))

;;
(?- (prove (mem ?x (1 2 3))))



