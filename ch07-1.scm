(require "./cl-emu")

;(require "./ch05-2") ;binding-var, binding-var
(define binding-var car)
(define binding-val cdr)

(require "./ch06-2")
(use srfi-1)
;(define-class <rule>...)
;(define-class <exp> ...)

(define (make-rule pattern response) (list pattern response))
(define rule-pattern first)
(define rule-response second)

(define (make-exp lhs op rhs)
  ;;(dbg :student "(make-exp lhs:~a op:~a rhs:~a)" lhs op rhs)
  (list op lhs rhs))
;(define mkexp make-exp)
(define exp-op first)
(define exp-lhs second)
(define exp-rhs third)
(define (exp? x) (list? x))
(define (exp-args x) (cdr x))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(define *student-rules*
  (map expand-pat-match-abbrev
       '(((?x* |.|)                  ?x)
         ((?x* |.| ?y*)          (?x ?y))
         ((if ?x* |,| then ?y*)  (?x ?y))
         ((if ?x* then ?y*)      (?x ?y))
         ((if ?x* |,| ?y*)       (?x ?y))
         ((?x* |,| and ?y*)      (?x ?y))
         ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
         ((find ?x*)             (= to-find ?x))
         ((?x* equals ?y*)       (= ?x ?y))
         ((?x* same as ?y*)      (= ?x ?y))
         ((?x* = ?y*)            (= ?x ?y))
         ((?x* is equal to ?y*)  (= ?x ?y))
         ((?x* is ?y*)           (= ?x ?y))
         ((?x* - ?y*)            (- ?x ?y))
         ((?x* minus ?y*)        (- ?x ?y))
         ((difference between ?x* and ?y*)  (- ?y ?x))
         ((difference ?x* and ?y*)          (- ?y ?x))
         ((?x* + ?y*)            (+ ?x ?y))
         ((?x* plus ?y*)         (+ ?x ?y))
         ((sum ?x* and ?y*)      (+ ?x ?y))
         ((product ?x* and ?y*)  (* ?x ?y))
         ((?x* * ?y*)            (* ?x ?y))
         ((?x* times ?y*)        (* ?x ?y))
         ((?x* / ?y*)            (/ ?x ?y))
         ((?x* per ?y*)          (/ ?x ?y))
         ((?x* divided by ?y*)   (/ ?x ?y))
         ((half ?x*)             (/ ?x 2))
         ((one half ?x*)         (/ ?x 2))
         ((twice ?x*)            (* 2 ?x))
         ((square ?x*)           (* ?x ?x))
         ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
         ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
         ((?x* % ?y*)            (* (/ ?x 100) ?y)))))
#;(map print *student-rules*)
(define (student words)
  ;; Solve certain Algebra Word Problems.
  (solve-equations
   (create-list-of-equations
    (translate-to-expression (remove noise-word? words)))))

(define (translate-to-expression words)
  ;; Translate an English phrase into an equation or expression.
  (or (rule-based-translator
       words *student-rules*
       :rule-if rule-pattern :rule-then rule-response
       :action (lambda (bindings response)
                 (cl:sublis (map translate-pair bindings)
							response)))
      (make-variable words)))

(define (translate-pair pair)
  ;; Translate the value part of the pair into an equation or expression.
  (cons (binding-var pair)
        (translate-to-expression (binding-val pair))))

(define (create-list-of-equations exp)
  ;; Separate out equations embedded in nested parens.
  (cond [(null? exp) '()]
        [(cl:atom (first exp)) (list exp)]
        [else (append (create-list-of-equations (first exp))
                      (create-list-of-equations (rest exp)))]))

(define (make-variable words)
  ;; Create a variable name based on the given list of words
  ;; - The list of words will already have noise words removed
  (first words))

(define (noise-word? word)
  ;; Is this a low-content word that can be safely ignored?
  (memq word '(a an the this number of $)))
