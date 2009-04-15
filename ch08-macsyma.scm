;;;; File macsyma.scm: a scheme implementation of MACSYMA in Chapter 8

;(require 'patmatch)
(require "./ch07-2")

;; other utils
(require "./ch06-2") ; %get, %set!

(define (exp-rhs-set! exp new-rhs)
  (set-cdr! (cdr exp) (list new-rhs))
  new-rhs)
(define (exp-rhs-inc! exp n)
  (exp-rhs-set! exp (+ (exp-rhs exp) n)))
(define (exp-rhs-dec! exp n)
  (exp-rhs-set! exp (- (exp-rhs exp) n)))

;;
(define (length=1? x)
  (and (pair? x) (null? (rest x))))

(define-macro (assert exp)
  `(dbg :assert #`"(assert ,|exp| => ~a)" ,exp))


;8.1
(define (variable? exp)
  (memq exp '(x y z m n o p q r s t u v w)))

;;; From student.lisp:
;(defstruct (rule (:type list)) pattern response)
;(defstruct (exp (:type list)
;                (:constructor mkexp (lhs op rhs)))
;  op lhs rhs)
;8.2
(define (make-rule pattern response) (list pattern response))
;8.1, 8.2
(define (rule-pattern rule) (first rule))
(define (rule-response rule) (second rule))
;8.2
(define (make-exp lhs op rhs) (list op lhs rhs))
(define (exp? x) (pair? x))
(define (exp-args x) (cdr x))
(define (exp-op x) (first x))
(define (exp-lhs x) (second x))
(define (exp-rhs x) (third x))
;8.2
(define (binary-exp? x)
  (and (exp? x) (= (length (exp-args x)) 2)))
;8.2
(define (prefix->infix exp)
  "Translate prefix to infix expressions."
  (if (cl:atom exp) exp
      (map prefix->infix
           (if (binary-exp? exp)
               (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
               exp))))

;; Define x+ and y+ as a sequence:
;8.1
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

; 8.3
;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n number?))
(pat-match-abbrev 'm '(?is m number?))
(pat-match-abbrev 's '(?is s not-number?))

;8.1, 8.4
(define *infix->prefix-rules*
  (map expand-pat-match-abbrev
       '(((x+ = y+) (= x y))
         ((- x+)    (- x))
         ((+ x+)    (+ x))
         ((x+ + y+) (+ x y))
         ((x+ - y+) (- x y))
         ((d y+ / d x) (d y x))        ;*** New rule
         ((Int y+ d x) (int y x))      ;*** New rule
         ((x+ * y+) (* x y))
         ((x+ / y+) (/ x y))
         ((x+ ^ y+) (^ x y)))))

; 8.1
(define (infix->prefix exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
;  (format #t "(infix->prefix exp:~a)\n" exp)
  (cond [(cl:atom exp) exp]
        [(= (length exp) 1) (infix->prefix (first exp))]
        [(rule-based-translator exp *infix->prefix-rules*
                                :rule-if rule-pattern
                                :rule-then rule-response
                                :action (lambda (bindings response)
                                          (cl:sublis (map
                                                      (lambda (pair)
                                                        (cons (car pair)
                                                              (infix->prefix (cdr pair))))
                                                      bindings)
                                                     response)))]
        [(symbol? (car exp))
         (list (car exp) (infix->prefix (cdr exp)))]
        [else (error "Illegal exp")]))

(define *simplification-rules* '()) ;Rules are in file macsymar.lisp

(define (^ x y) "Exponentiation" (expt x y))

;8.2, p.244
(define (simplifier)
  "Read a mathematical expression, simplify it, and print the result."
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
  "Simplify an expression by first simplifying its components."
  (if (cl:atom exp) exp
      (simplify-exp (map simplify exp))))

#;(define (simplify-exp exp)
  "Simplify using a rule, or by doing arithmetic."
  (cond [(rule-based-translator exp *simplification-rules*
                                :rule-if exp-lhs :rule-then exp-rhs
                                :action (lambda (bindings response)
                                          (simplify (cl:sublis bindings response))) )]
        [(evaluable? exp) (eval exp (interaction-environment))]
        [else exp]))
;;; simplify-exp is redefined below
;(defun simplify-exp (exp)
;  "Simplify using a rule, or by doing arithmetic."
;  (cond ((rule-based-translator exp *simplification-rules*
;           :rule-if #'exp-lhs :rule-then #'exp-rhs
;           :action #'(lambda (bindings response)
;                       (simplify (sublis bindings response)))))
;        ((evaluable exp) (eval exp))
;        (t exp)))

(define (evaluable? exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every number? (exp-args exp))
       (or (memq (exp-op exp) '(+ - * /))
           (and (eq? (exp-op exp) '^)
                (integer? (second (exp-args exp)))))))

;8.3
(define (not-number? x) (not (number? x)))
;8.3
(define (simp-rule rule)
  "Transform a rule into proper format."
  (let1 exp (infix->prefix rule)
    (make-exp (expand-pat-match-abbrev (exp-lhs exp))
              (exp-op exp) (exp-rhs exp))))

;8.6
(define (simp-fn op)
;  (format #t "(simp-fn op:~a) => ~a\n" op (%get op 'simp-fn))
  (%get op 'simp-fn))
(define (set-simp-fn op fn) (%set! op 'simp-fn fn))

;8.6
(define (simplify-exp exp)
  "Simplify using a rule, or by doing arithmetic,
   or by using the simp function supplied for this operator."
;  (format #t "(simplify-exp exp:~a)\n" exp)
  (cond [(simplify-by-fn exp)] ;***
;       => identity] ;;
        [(rule-based-translator exp *simplification-rules*
                                :rule-if exp-lhs
                                :rule-then exp-rhs
                                :action (lambda (bindings response)
                                          (simplify (cl:sublis bindings response))))]
                                        ;         => identity]
        [(evaluable? exp) (eval exp (interaction-environment))]
        [else exp]))
;8.6
(define (simplify-by-fn exp)
  "If there is a simplification fn for this exp,
   and if applying it gives a non-null result,
   then simplify the result and return that."
;  (format #t "(simplify-by-fn, exp:~a => fn for ~a:~a)\n" exp (exp-op exp) (simp-fn (exp-op exp)))
  (let* ((fn (simp-fn (exp-op exp)))
         (result (if fn (funcall fn exp) #f)))
    (if (cl:thru result)
        (simplify result)
        #f)));; nil
#|  (and-let* ([fn (simp-fn (exp-op exp))]
             [result (funcall fn exp)])
    (if (null? result) #f
        (simplify result))))
|#
;8.6, p.254
(define (factorize exp)
  "Return a list of the factors of exp^n,
   where each factor is of the form (^ y n)."
;  (format #t "(factorize ~a)\n" exp)
  (let ([factors '()]
        [constant 1])
    (letrec ((fac (lambda (x n)
                    ;(format #t "(fac x:~a n:~a)\n" x n)
                    (cond
                     [(number? x)
                      (set! constant (* constant (expt x n)))]
                     [(starts-with x '*)
                      (fac (exp-lhs x) n)
                      (fac (exp-rhs x) n)]
                     [(starts-with x '/)
                      (fac (exp-lhs x) n)
                      (fac (exp-rhs x) (- n))]
                     [(and (starts-with x '-) (length=1? (exp-args x)))
                      (set! constant (- constant))
                      (fac (exp-lhs x) n)]
                     [(and (starts-with x '^) (number? (exp-rhs x)))
                      (fac (exp-lhs x) (* n (exp-rhs x)))]
                     [else (let1 factor (cl:find x factors :key exp-lhs :test equal?)
                             (if (cl:thru factor)
                                 (exp-rhs-inc! factor n)
                                 (cl:push `(^ ,x ,n) factors)))]
                     ))))
      (fac exp 1)
      #;(format #t " ;; constant: ~a, factors: ~a\n" constant factors)
      (case constant
        [(0) '((^ 0 1))]
        [(1) factors]
        [else `((^ ,constant 1) ,@factors)]
        ))))
;8.6
(define (unfactorize factors)
  "Convert a list of factors back into prefix form."
  ;(format #t "(unfactorize ~a)\n" factors)
  (cond [(null? factors) 1]
        [(length=1? factors) (car factors)]
        [else `(* ,(car factors) ,(unfactorize (cdr factors)))]))
;8.6
(define (divide-factors numer denom)
  "Divide a list of factors by another, producing a third."
  ;(format #t "(divide-factors numer:~a denom:~a)\n" numer denom)
  (let1 result (map cl:copy-list numer)
    #;(format #t " :: result: ~a " result)
    (dolist (d denom)
      (let1 factor (cl:find (exp-lhs d) result :key exp-lhs :test equal?)
        (if (cl:thru factor)
            (begin
              #;(format #t " // try to decr ~a with ~a\n"
                      (exp-rhs factor) (exp-rhs d))
              (exp-rhs-dec! factor (exp-rhs d))
              )
            (begin
              #;(format #t " // try to push ~a to result\n"
                      `(^ ,(exp-lhs d) ,(- (exp-rhs d))) )
              (cl:push `(^ ,(exp-lhs d) ,(- (exp-rhs d))) result)
              )
            )))
    ;(format #t "  => ~a\n" result)
    (remove (lambda (el) (zero? (exp-rhs el))) result) ))
;   #?=(cl:delete 0 #?=result :key exp-rhs)))
;8.6
(define (free-of exp var)
  "True if expression has no occurrence of var."
  #;(format #t "(free-of exp:~a var:~a) => ~a\n" exp var
          (not (find-anywhere var exp)))
  (not (find-anywhere var exp)))
;8.6
(define (find-anywhere item tree)
  "Does item occur anywhere in tree?  If so, return it."
  ;(format #t "(find-anywhere item:~a in tree:~a)..\n" item tree)
  (cond [(eqv? item tree) tree]
        [(cl:atom tree) #f]
        ;[(null? tree) #f]
        #;[else (or (find-anywhere item (first tree))
                  (find-anywhere item (rest tree)))]
        [(find-anywhere item (car tree))]
        [(find-anywhere item (cdr tree))]
        [else #f]
        ))

;8.6, p.256
(define (integrate exp x)
  ;; First try some trivial cases
  ;(format #t "(integrate exp:~a x:~a)\n" exp x)
  (cond [(free-of exp x) `(* ,exp x)] ;; Int c dx = c*x
        [(starts-with exp '+)         ;; Int f + g = Int f + Int g
         `(+ ,(integrate (exp-lhs exp) x)
             ,(integrate (exp-rhs exp) x))]
        [(starts-with exp '-)
         (case (length (exp-args exp))
           [(1) (integrate (exp-lhs exp) x)]     ;; Int - f = - Int f
           ;[(1) `(- ,(integrate (exp-lhs exp) x))]     ;; Int - f = - Int f
           [(2) `(- ,(integrate (exp-lhs exp) x) ;; Int f - g = Int f - Int g
                    ,(integrate (exp-rhs exp) x))])]
        ;; Now move the constant factors to the left of the integral
        [(receive (const-factors x-factors)
             (partition-if (cut free-of <> x) (factorize exp))
           (identity ;simplify !!!!!!!!!!!!
            `(* ,(unfactorize const-factors)
                ;; And try to integrate:
                ,(cond [(null? x-factors) x]
                       [(any (lambda (factor) (deriv-divides factor x-factors x))
                             x-factors)]
                       ;; <other methods here>
                       [else `(int? ,(unfactorize x-factors) ,x)]
                       ))))
         ]
        [else #f]
        ))

;8.6
(define (partition-if pred lis)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  ;(format #t "(partition-if pred:~a list:~a)\n" pred lis)
  (let ([yes-list '()]
        [no-list '()])
    (dolist (item lis)
      (if (funcall pred item)
          (cl:push item yes-list)
          (cl:push item no-list)))
    (values (reverse! yes-list) (reverse! no-list))))

;8.6, p.257
(define (deriv-divides factor factors x)
  ;(format #t "(deriv-divides factor:~a factors:~a x:~a)\n" factor factors x)
  (assert (starts-with factor '^))
  (let* ([u (exp-lhs factor)] ;;factor = u^n
         [n (exp-rhs factor)]
         [k (divide-factors
             factors (factorize `(* ,factor ,(deriv u x))))])
    (cond [(free-of k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n != -1
           ;;                    = k*log(u)        for n = -1
           (if (= n -1)
               `(* ,(unfactorize k) (log ,u))
               `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
                   ,(+ n 1)))]
          [(and (= n 1) (in-integral-table? u))
           ;; Int y*f(y) dx = Int f(y) dy
           (let1 k2 (divide-factors
                     factors
                     (factorize `(* ,u ,(deriv (exp-lhs u) x))))
             (if (free-of k2 x)
                 `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
                     ,(unfactorize k2))
                 #f))]
          [else #f])))
;8.6
(define (deriv y x) (simplify `(d ,y ,x)))

;8.6
(define (integration-table rules)
  (dolist (i-rule rules)
    ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
    (let1 rule (simp-rule i-rule) ;; s/infix->prefix/simp-rule/
      (%set! (exp-op (exp-lhs (exp-lhs rule))) 'int rule))))
;8.6
(define (in-integral-table? exp)
  (and (exp? exp) (%get (exp-op exp) 'int)))
;8.6
(define (integrate-from-table op arg)
  (let1 rule (%get op 'int)
    (cl:subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))

#;(set-simp-fn 'Int 'integrate)
#;(set-simp-fn 'int (lambda (exp)
                    (integrate (exp-lhs exp) (exp-rhs exp))))
(set-simp-fn 'int (lambda (exp)
                    (unfactorize
                     (factorize
                      (integrate (exp-lhs exp) (exp-rhs exp))))))
;;
;; macsymar.lisp
;;
;8.2
(define *simplification-rules*
  (map simp-rule '(;;infix->prefix '(
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
;8.3
(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '((s * n = n * s)
                     (n * (m * x) = (n * m) * x)
                     (x * (n * y) = n * (x * y))
                     ((n * x) * y = n * (x * y))
                     (n + s = s + n)
                     ((x + m) + n = x + n + m)
                     (x + (y + n) = (x + y) + n)
                     ((x + n) + y = (x + y) + n)))))
;8.4
(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '((log 1         = 0)
                     (log 0         = undefined)
                     (log e         = 1)
                     (sin 0         = 0)
                     (sin pi        = 0)
                     (cos 0         = 1)
                     (cos pi        = -1)
                     (sin(pi / 2)   = 1)
                     (cos(pi / 2)   = 0)
                     (log (e ^ x)   = x)
                     (e ^ (log x)   = x)
                     ((x ^ y) * (x ^ z) = x ^ (y + z))
                     ((x ^ y) / (x ^ z) = x ^ (y - z))
                     (log x + log y = log(x * y))
                     (log x - log y = log(x / y))
                     ((sin x) ^ 2 + (cos x) ^ 2 = 1)
                     ))))
;8.4後半
(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '((d x / d x       = 1)
                     (d (u + v) / d x = (d u / d x) + (d v / d x))
                     (d (u - v) / d x = (d u / d x) - (d v / d x))
                     (d (- u) / d x   = - (d u / d x))
                     (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
                     (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) 
                        / v ^ 2) ; [This corrects an error in the first printing]
                     (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
                     (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                        + u ^ v * (log u) * (d v / d x))
                     (d (log u) / d x = (d u / d x) / u)
                     (d (sin u) / d x = (cos u) * (d u / d x))
                     (d (cos u) / d x = - (sin u) * (d u / d x))
                     (d (e ^ u) / d x = (e ^ u) * (d u / d x))
                     (d u / d x       = 0)))))
;;8.6
(integration-table
  '((Int log(x) d x = x * log(x) - x)
    (Int exp(x) d x = exp(x))
    (Int sin(x) d x = - cos(x))
    (Int cos(x) d x = sin(x))
    (Int tan(x) d x = - log(cos(x)))
    (Int sinh(x) d x = cosh(x))
    (Int cosh(x) d x = sinh(x))
    (Int tanh(x) d x = log(cosh(x)))
    ))
