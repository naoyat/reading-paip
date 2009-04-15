(require "./ch08-1")
;;
(define (make-rule pattern response) (list pattern response))
(define (rule-pattern rule) (first rule))
(define (rule-response rule) (second rule))

(define (make-exp lhs op rhs) (list op lhs rhs))
(define (exp? x) (pair? x))
(define (exp-args x) (rest x))
(define (exp-op exp) (first exp))
(define (exp-lhs exp) (second exp))
(define (exp-rhs exp) (third exp))

(define (binary-exp? x)
  (and (exp? x) (= (length (exp-args x)) 2)))

(define (prefix->infix exp)
  "Translate prefix to infix expressions."
  (if (cl:atom exp) exp
      (map prefix->infix
           (if (binary-exp? exp)
               (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
               exp))))

(define *simplification-rules*
  (map infix->prefix '(
                       (x + 0  = x)
                       (0 + x  = x)
                       (x + x  = 2 * x)
                       (x - 0  = x)
                       (0 - x  = - x)
                       (x - x  = 0)
                       (- - x  = x)
                       (x * 1  = x)
                       (1 * x  = x)
                       (x * 0  = 0)
                       (0 * x  = 0)
                       (x * x  = x ^ 2)
                       (x / 0  = undefined)
                       (0 / x  = 0)
                       (x / 1  = x)
                       (x / x  = 1)
                       (0 ^ 0  = undefined)
                       (x ^ 0  = 1)
                       (0 ^ x  = 0)
                       (1 ^ x  = 1)
                       (x ^ 1  = x)
                       (x ^ -1 = 1 / x)
                       (x * (y / x) = y)
                       ((y / x) * x = y)
                       ((y * x) / x = y)
                       ((x * y) / x = y)
                       (x + - x = 0)
                       ((- x) + x = 0)
                       (x + y - x = y)
                       )))

(define (^ x y) (expt x y))

;; p.244
(define (simplifier)
  (let loop ()
    (display "simplifier> ") (flush)
    (let1 input (read)
      (if (eof-object? input)
          (print "bye.")
          (begin (print (simp input))
                 (loop))))))

(define (simp inf)
  (prefix->infix (simplify (infix->prefix inf))))

(define (simplify exp)
  (if (cl:atom exp) exp
      (simplify-exp (map simplify exp))))

(define (simplify-exp exp)
  (cond [(rule-based-translator exp *simplification-rules*
                                :rule-if exp-lhs :rule-then exp-rhs
                                :action (lambda (bindings response)
                                          (simplify (cl:sublis bindings response))) )]
        [(evaluable? exp) (eval exp (interaction-environment))]
        [else exp]))

(define (evaluable? exp)
  (and (every number? (exp-args exp))
       (or (memq (exp-op exp) '(+ - * /))
           (and (eq? (exp-op exp) '^)
                (integer? (second (exp-args exp)))))))
