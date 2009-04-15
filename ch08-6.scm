(require "./ch06-2") ; %get, %set!
(require "./ch08-4")

(define (exp-rhs-set! exp new-rhs)
  (set-cdr! (cdr exp) new-rhs)
  new-rhs)
(define (exp-rhs-inc! exp n)
  (exp-rhs-set! exp (+ (exp-rhs exp) n)))
(define (exp-rhs-dec! exp n)
  (exp-rhs-set! exp (- (exp-rhs exp) n)))

(define (simp-fn op)
;  (format #t "(simp-fn op:~a) => ~a\n" op (%get op 'simp-fn))
  (%get op 'simp-fn))
(define (set-simp-fn op fn) (%set! op 'simp-fn fn))

(define (simplify-exp exp)
  "Simplify using a rule,
    or by doing arithmetic,
    or by using the simp function supplied for this operator."
;  (format #t "(simplify-exp exp:~a)\n" exp)
  (cond [(simplify-by-fn exp)] ;***
;		=> identity] ;;
        [(rule-based-translator exp *simplification-rules*
                                :rule-if exp-lhs
								:rule-then exp-rhs
                                :action (lambda (bindings response)
                                          (simplify (cl:sublis bindings response))))]
										;         => identity]
        [(evaluable? exp) (eval exp (interaction-environment))]
        [else exp]))

(define (simplify-by-fn exp)
  "If there is a simplification fn for this exp,
   and if applying it gives a non-null result,
   then simplify the result and return that."
;  (format #t "(simplify-by-fn, exp:~a => fn for ~a:~a)\n" exp (exp-op exp) (simp-fn (exp-op exp)))
  (let* ((fn (simp-fn (exp-op exp)))
		 (result (if fn (funcall fn exp) #f)))
	(if (cl:thru result)
		(simplify result)
		#f)));cl:nil)))
#|  (and-let* ([fn (simp-fn (exp-op exp))]
             [result (funcall fn exp)])
    (if (null? result) #f
        (simplify result))))
|#

;;p.254
(define (factorize exp)
  "Return a list of the factors of exp^n,
   where each factor is of the form (^ y n)."
;  (format #t "(factorize ~a)\n" exp)
  (let ([factors '()]
        [constant 1])
	(letrec ((fac (lambda (x n)
					;(format #t "(fac x:~a n:~a)\n" x n)
					(cond [(number? x)
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
								  #;(format #t "(find x:~a factors:~a key:exp-lhs:~a test:equal)\n"
										  x factors (map exp-lhs factors))
								  (if (cl:thru factor)
									  ;(begin
										;(format #t " 1> ~a\n" (exp-rhs factor))
										;(inc! (exp-rhs factor) n)
										;(format #t " 2> ~a\n" (exp-rhs factor))
										(exp-rhs-inc! factor n)
										;(format #t " 3> ~a\n" (exp-rhs factor))
										;)
									  (cl:push `(^ ,x ,n) factors)))]
						  )) ))
	  (fac exp 1)
	  (case constant
		[(0) '((^ 0 1))]
		[(1) factors]
		[else `((^ ,constant 1) . ,factors)]
		))))

(define (unfactorize factors)
  ;(format #t "(unfactorize ~a)\n" factors)
  (cond [(null? factors) 1]
        [(length=1? factors) (first factors)]
        [else `(* ,(first factors) ,(unfactorize (rest factors)))]))

(define (divide-factors numer denom)
  (format #t "(divide-factors numer:~a denom:~a)\n" numer denom)
  (let1 result (map cl:copy-list numer)
	(dolist (d denom)
	  (let1 factor (cl:find (exp-lhs d) result :key exp-lhs :test equal?)
		(format #t "(find ~a ~a...)\n"
				(if (cl:thru factor)
										;(dec! (exp-rhs factor) (exp-rhs d))
					(exp-rhs-dec! factor (exp-rhs d))
					(cl:push `(^ ,(exp-lhs d) ,(- (exp-rhs d))) result))))
	  (cl:delete 0 result :key exp-rhs) )) )

(define (free-of exp var)
  #;(format #t "(free-of exp:~a var:~a) => ~a\n" exp var
		  (not (find-anywhere var exp)))
  (not (find-anywhere var exp)))

(define (find-anywhere item tree)
  "Does item occur anywhere in tree?  If so, return it."
  ;(format #t "(find-anywhere item:~a in tree:~a)..\n" item tree)
  (cond [(eqv? item tree) tree]
		[(cl:atom tree) #f]
		[(null? tree) #f]
		[else (or (find-anywhere item (first tree))
				  (find-anywhere item (rest tree)))]
		#;[(find-anywhere item (first tree))]
		#;[(find-anywhere item (rest tree))]
		))

(define (length=1? x)
  (and (pair? x) (null? (rest x))))

;;p.256
(define (integrate exp x)
  ;; First try some trivial cases
  (format #t "(integrate exp:~a x:~a)\n" exp x)
  (cond [(free-of exp x) `(* ,exp x)] ;; Int c dx = c*x
        [(starts-with exp '+)         ;; Int f + g = Int f + Int g
         `(+ ,(integrate (exp-lhs exp) x)
             ,(integrate (exp-rhs exp) x))]
        [(starts-with exp '-)
         (case (length (exp-args exp))
           [(1) (integrate (exp-lhs exp) x)]     ;; Int - f = - Int f
           [(2) `(- ,(integrate (exp-lhs exp) x) ;; Int f - g = Int f - Int g
                    ,(integrate (exp-rhs exp) x))])]
		;; Now move the constant factors to the left of the integral
        [(receive (const-factors x-factors)
			 (partition-if (cut free-of <> x) (factorize exp))
           (simplify
            `(* ,(unfactorize const-factors)
				;; And try to integrate:
                ,(cond [(null? x-factors) x]
                       [(cl:some (lambda (factor) (deriv-divides factor x-factors x))
                             x-factors)]
					   ;; <other methods here>
                       [else `(int? ,(unfactorize x-factors) ,x)]
					   ))))
		 ]
		[else #f]
		))

(define (partition-if pred lis)
  ;; Return 2 values: elements of list that satisfy pred.
  ;(format #t "(partition-if pred:~a list:~a)\n" pred lis)
  (let ([yes-list '()]
        [no-list '()])
    (dolist (item lis)
      (if (funcall pred item)
          (cl:push item yes-list)
          (cl:push item no-list)))
    (values (reverse! yes-list) (reverse! no-list))))

;; p.257
(define (deriv-divides factor factors x)
  ;(format #t "(deriv-divides factor:~a factors:~a x:~a)\n" factor factors x)
  ;(assert (starts-with factor '^))
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

(define (deriv y x)
;  (format #t "(deriv y:~a x:~a)\n" y x)
  (simplify `(d ,y ,x)))

(define (integration-table rules)
  (dolist (i-rule rules)
    (let1 rule (infix->prefix i-rule)
	  #;(format #t "∫: (set ~a, int, ~a => ~a)\n"
			  (exp-op (exp-lhs (exp-lhs rule))) i-rule rule)
      (%set! (exp-op (exp-lhs (exp-lhs rule))) 'int rule))))

(define (in-integral-table? exp)
  (and (exp? exp) (%get (exp-op exp) 'int)))

(define (integrate-from-table op arg)
  (let1 rule (%get op 'int)
    (cl:subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))

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

;(set-simp-fn 'Int 'integrate)
(set-simp-fn 'int (lambda (exp)
                    (integrate (exp-lhs exp) (exp-rhs exp))))
;(set-simp-fn 'Int (lambda (exp)
; (unfactorize (factorize (integrate (exp-lhs exp) (exp-rhs exp))))))
