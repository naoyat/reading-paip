(require "./cl-emu")
;;;;;;;;;
(define (starts-with lis x)
  ;; Is this a list whose first element is x?
  ;(format #t "(starts-with lis:~a x:~a)\n" lis x)
  (and (pair? lis) (eqv? (car lis) x)))

;;;;;;;;;
(define (simple-equal? x y)
  ;; Are x and y equal? (Don't check inside strings.)
#|
  (if (or (cl:atom x) (cl:atom y))
      (eqv? x y)
      (and (simple-equal? (first x) (first y))
           (simple-equal? (rest x) (rest y)))))
|#
  (if (and (pair? x) (pair? y))
      (and (simple-equal? (first x) (first y))
           (simple-equal? (rest x) (rest y)))
      (eqv? x y))
  )

(define (pat-match pattern input)
  ;; Does pattern match input? Any variable can match anything.
  (if (variable? pattern)
      #t
#|    (if (or (cl:atom pattern) (cl:atom input))
          (eqv? pattern input)
          (and (pat-match (first pattern) (first input))
               (pat-match (rest pattern) (rest input)))))) |#
      (if (and (pair? pattern) (pair? input))
          (and (pat-match (first pattern) (first input))
               (pat-match (rest pattern) (rest input)) );fi
          (eqv? pattern input))
      ))


(define (variable? x)
  ;; Is x a variable (a symbol beginning with '?')?
  #;(and (symbol? x) (equal? (char (symbol-name x) 0) #\?))
  (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?)))

;#?=(pat-match '(I need a ?X) '(I need a vacation))
;#?=(pat-match '(I need a ?X) '(I really need a vacation))

#;(define (pat-match pattern input)
  ;; Does pattern match input? Any variable can match anything.
  (if (variable? pattern)
      #t
      (every pat-match pattern input)))
#|
;;Exercise 5.1 [s]
;(every pat-match pattern input)
#?=(every pat-match '(a b c) '(a))
#?=(every pat-match '(I need a ?X) '(I need a vacation))
#?=(every pat-match '(I need a ?X) '(I really need a vacation))
|#
;(pat-match '(a b c) '(a))
;(pat-match '(I need a ?X) '(I need a vacation))
;(pat-match '(I need a ?X) '(I really need a vacation))

#;(sublis '((?X . vacation))
        '(what would it mean to you if you got a ?X ?))

#;(define (pat-match pattern input)
  ;; Does pattern match input?
  ;; WARNING: buggy version
  (if (variable? pattern)
      (list (cons pattern input))
      (if (and (pair? pattern) (pair? input))
          (append (pat-match (first pattern) (first input))
                  (pat-match (rest pattern) (rest input)))
          (eqv? pattern input))))

;#?=(pat-match '(I need a ?X) '(I need a vacation))
;#?=(pat-match '(I need a ?X) '(I really need a vacation))

(define fail cl:nil ); Indicates pat-match failure
(define no-bindings '((#t . #t)) ); Indicates pat-match success, with no variables

(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
  (assoc var bindings))
(define (binding-val binding)
  ;; Get the value part of a single binding
  (cdr binding))
(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (binding-val (get-binding var bindings)))
(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (cons (cons var val) bindings))

(define (pat-match pattern input . args)
  (let-optionals* args ((bindings no-bindings))
    (cond [(eq? bindings fail) fail]
          [(variable? pattern) (match-variable pattern input bindings)]
          [(eqv? pattern input) bindings]
          [(and (pair? pattern) (pair? input))
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input)
                                 bindings))]
          [else fail])))

(define (match-variable var input bindings)
  (let1 binding (get-binding var bindings)
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

;#?=(pat-match '(i need a ?X) '(i need a vacation))

(define (extend-bindings var val bindings)
  (cons (cons var val)
        (if (eq? bindings no-bindings)
            '()
            bindings)))
#|
#?=(sublis (pat-match '(i need a ?X) '(i need a vacation))
           '(what would it mean to you if you got a ?X ?))
#?=(pat-match '(i need a ?X) '(i really need a vacation))
#?=(pat-match '(this is easy) '(this is easy))
#?=(pat-match '(?X is ?X) '((2 + 2) is 4))
#?=(pat-match '(?X is ?X) '((2 + 2) is (2 + 2)))
#?=(pat-match '(?P need . ?X) '(i need a long vacation))
|#
(define (pat-match pattern input . args)
  (let-optionals* args ((bindings no-bindings))
    (cond [(eq? bindings fail) fail]
          [(variable? pattern) (match-variable pattern input bindings)]
          [(eqv? pattern input) bindings]
          [(segment-pattern? pattern)
           (segment-match pattern input bindings)]
          [(and (pair? pattern) (pair? input))
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input)
                                 bindings))]
          [else fail])))

(define (segment-pattern? pattern)
  (and (pair? pattern)
       (starts-with (first pattern) '?*)))

(define (segment-match pattern input bindings . args)
  (let-optionals* args ((start 0))
    (let ([var (second (first pattern))]
          [pat (rest pattern)])
      (if (null? pat)
          (match-variable var input bindings)
          ;; We assume that pat starts with a constant
          ;; In other words, a pattern cannot have 2 consecutive vars
          (let1 pos (cl:position (first pat) input
                                 :start start :test equal?)
            (if pos
                (let1 b2 (pat-match pat (subseq input pos) bindings)
                  (if (eq? b2 fail)
                      (segment-match pattern input bindings (+ pos 1))
                      (match-variable var (subseq input 0 pos) b2)))
                fail))))))

#;(pat-match '((?* ?p) need (?* ?x))
              '(Mr Hulot and I need a vacation))
#;(pat-match '((?* ?x) is a (?* ?y))
              '(what he is is a fool))

;#?=(pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))

;;;;;;
(define (segment-match pattern input bindings . args)
  (let-optionals* args ((start 0))
    (let ([var (second (first pattern))] ;cadar
          [pat (rest pattern)])
      (if (null? pat)
          (match-variable var input bindings)
          ;; We assume that pat starts with a constant
          ;; In other words, a pattern cannot have 2 consecutive vars
          (let1 pos (cl:position (first pat) input
                                 :start start :test equal?)
			(if pos
                (let1 b2 (pat-match pat (subseq input pos)
                                    ;; bindings)
                                    (match-variable var (subseq input 0 pos) bindings))
                  (if (eq? b2 fail)
                      (segment-match pattern input bindings (+ pos 1))
                      ;; (match-variable var (subseq input 0 pos) b2)))))))))
					  b2))
				fail))))))

;#?=(pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
;#?=(pat-match '((?* ?x) a c (?* ?x)) '(1 2 a b a b 1 2 a b))
