(require "./ch07-2")
#;(define (infix->prefix infix-exp)
  (prefix->infix infix-exp))

(define (infix->prefix exp)
;  (format #t "(infix->prefix exp:~a)\n" exp)
  (cond [(cl:atom exp) exp]
        [(= (length exp) 1) (infix->prefix (first exp))]
        [(rule-based-translator exp *infix->prefix-rules*
                                :rule-if rule-pattern
                                :rule-then rule-response
                                :action (lambda (bindings response)
                                          (cl:sublis (map (lambda (pair)
                                                            (cons (first pair)
                                                                  (infix->prefix (rest pair))))
                                                          bindings)
                                                     response)))]
        [(symbol? (first exp))
         (list (first exp) (infix->prefix (rest exp)))]
        [else (error "Illegal exp")]))

(define (variable? exp)
  (memq exp '(x y z m n o p q r s t u v w)))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(define (rule-pattern rule) (first rule))
(define (rule-response rule) (second rule))

(define *infix->prefix-rules*
  (map expand-pat-match-abbrev
       '(((x+ = y+) (= x y))
         ((- x+)    (- x))
         ((+ x+)    (+ x))
         ((x+ + y+) (+ x y))
         ((x+ - y+) (- x y))
         ((x+ * y+) (* x y))
         ((x+ / y+) (/ x y))
         ((x+ ^ y+) (^ x y)))))
