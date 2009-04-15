(require "./ch07-1")

(use slib)
(require 'trace)

;;
;; 7.2
(define (solve-equations equations)
  ;; Print the equations and their solution
  (newline)
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations '())) )

(define (solve equations known)
  ;; Solve a system of equations by constraint propagation.
  ;; - Try to solve for one equation, and substitute its value into the others.
  ;;   If that doesn't work, return what is known.
  (dbg :student "(solve equations:~a known:~a)" equations known)
  (or (any (lambda (equation)
			 #;(format #t "EQ: ~a\n" equation)
             (and-let* ([x (one-unknown equation)]
                        [answer (solve-arithmetic (isolate equation x))])
			   #;(format #t " => x: ~a, answer:~a\n" x answer)
			   #;(format #t " => (subst ~a ~a ~a) => ~a\n"
					   (exp-rhs answer) (exp-lhs answer) (cl:remove equation equations)
					   (cl:subst (exp-rhs answer) (exp-lhs answer)
								 (cl:remove equation equations)))
			   (solve (cl:subst (exp-rhs answer) (exp-lhs answer)
								(cl:remove equation equations))
                      (cons answer known))))
           equations)
      known))

(define (isolate e x)
  ;; Isolate the lone x in e on the left-hand side of e.
  ;; - This assumes there is exactly one x in e,
  ;;   and that e is an equation.
  ;(dbg :student "(isolate e:~a x:~a)" e x)
  (cond [(eq? (exp-lhs e) x)
         ;; Case I: X = A --> X = n
         e]

        [(in-exp x (exp-rhs e))
         ;; Case II: A = f(X) --> f(X) = A
         (isolate (make-exp (exp-rhs e) '= (exp-lhs e)) x)]

        [(in-exp x (exp-lhs (exp-lhs e)))
         ;; Case III: f(X)*A = B --> f(X) = B/A
         (isolate (make-exp (exp-lhs (exp-lhs e)) '=
                            (make-exp (exp-rhs e)
                                      (inverse-op (exp-op (exp-lhs e)))
                                      (exp-rhs (exp-lhs e)))) x)]
        
        [(commutative? (exp-op (exp-lhs e)))
         ;; Case IV: A*f(X) = B --> f(X) = B/A
         (isolate (make-exp (exp-rhs (exp-lhs e)) '=
                            (make-exp (exp-rhs e)
                                      (inverse-op (exp-op (exp-lhs e)))
                                      (exp-lhs (exp-lhs e)))) x)]
        
        [else
         ;; Case V: A/f(X) = B --> f(X) = A/B
         (isolate (make-exp (exp-rhs (exp-lhs e)) '=
                            (make-exp (exp-lhs (exp-lhs e))
                                      (exp-op (exp-lhs e))
                                      (exp-rhs e))) x)]
        ))

;; p.228
(define (list->string-without-parens lis)
  (string-join (map x->string lis) " "))

(define (print-equations header equations)
  ;; Print a list of equations.
;  (format #t "~a  ~a" ;; "~%~a~{~%  ~{ ~a~}~}~%"
;         header (map prefix->infix equations))
  (print header)
  (map (cut format #t "   ~a\n" <>)
       (map list->string-without-parens (map prefix->infix equations))))

(define operators-and-inverses '((+ -) (- +) (* /) (/ *) (= =)))

(define (inverse-op op)
  (second (assoc op operators-and-inverses)))

(define (unknown? exp) (symbol? exp))

(define (in-exp x exp)
  ;; True if x appears anywhere in exp
  (or (eq? x exp)
      (and (exp? exp)
           (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(define (no-unknown exp)
  ;; Returns true if there are no unknowns in exp.
  (cond [(unknown? exp) #f]
        [(cl:atom exp) #t]
        [(no-unknown (exp-lhs exp)) (no-unknown (exp-rhs exp))]
        [else #f]))

;; p.229
(define (one-unknown exp)
  ;; Returns the single unknown in exp, if there is exactly one.
  (cond [(unknown? exp) exp]
        [(cl:atom exp) #f]
        [(no-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp))]
        [(no-unknown (exp-rhs exp)) (one-unknown (exp-lhs exp))]
        [else #f]))

(define (commutative? op)
  ;; Is operator commutative?
  (member op '(+ * =)))

(define (solve-arithmetic equation)
  ;; Do the arithmetic for the right-hand side.
  ;; - This assumes that the right-hand side is in the right form.
  (dbg :student "(solve-arithmetic equation:~a)" equation)
  (make-exp (exp-lhs equation) '= (eval (exp-rhs equation) (interaction-environment))))

(define (binary-exp? x)
  (and (exp? x) (= (length (exp-args x)) 2)))

(define (prefix->infix exp)
  ;; Translate prefix to infix expressions.
  (if (cl:atom exp) exp
      (map prefix->infix
           (if (binary-exp? exp)
               (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
               exp))))
