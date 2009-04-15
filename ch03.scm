(require "./cl-emu")

(define (fn1 x)
  (case x
    ((1) 10)
    ((2) 10)) )
#|
(print (fn1 1))

(define (fn2 x)
  (case (class-of x)
    ((#<class <integer>> #<class <real>>) (abs x))
    ((#<class <pair>>) (length x))
    (else (class-of x))
    ))
|#
(define (fn2 x)
  (cond
   [(is-a? x <number>) (abs x)] ;; integer > rational > real > complex > number
   [(is-a? x <pair>) (length x)]
   ))
;;数値オブジェクトのクラス階層を構成します。<complex>は <number>を継承し、<real>は<complex>を継承し、 <rational>は<real>を継承し、 <integer>は<rational>を継承します。 
#|
(print (fn2 3))
(print (fn2 3.14))
(print (fn2 '(1 2 3)))
|#
;;Exercise 3.1
#|
(let* ((x 6)
       (y (* x x)))
  (+ x y))
=>
((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)
|#
;;3.3
(define princ display);;とりあえず

(define (print33 exp)
  (cond [(pair? exp)
         (princ "(")
         (princ (car exp))
         (princ " . ")
         (print* (cdr exp))
         (princ ")")
         ]
        [else
         (princ exp)]
        )
  )
#|
(print33 1)
(newline)

(print33 '(1 2 3))
(newline)
|#

(define (print34 exp)
  (define (print-pair exp)
    (cond [(null? exp) 'empty]
          [(pair? exp)
           (princ (car exp))
           (unless (null? (cdr exp))
             (princ " ")
             (print-pair (cdr exp)))]
          [else
           (princ ". ")
           (princ exp)]))
  (cond [(pair? exp)
         (princ "(")
         (print-pair exp)
         (princ ")")]
        [else (princ exp)] ))

#|
(print34 '(1 . 2))
(newline)
(print34 '(1 2 . 3))
(newline)
(print34 '(1 2 3))
(newline)
|#
;;;;;;;;;
#|
(define (copy-tree tree)
  (cond [(null? tree) '()]
        [(pair? tree) (cons (car tree)
                            (copy-tree (cdr tree)))]
        [else tree]
        ))
|#
(define (tree-equal tree1 tree2 . opt)
  (equal? tree1 tree2))

(define tree '((a b) ((c)) (d e)))
#|
(print tree)
(print (copy-tree tree))
|#
(use gauche.test)
(test* "" #t (tree-equal tree (copy-tree tree)) )

(define (same-shape-tree a b)
  (tree-equal a b true))
(define (true) #t)

(test* "" #t (same-shape-tree tree '((1 2) ((3)) (4 5))) )
(test* "" #f (same-shape-tree tree '((1 2) (3) (4 5))) )

(define (english->french words)
  (sublis '[(are . va)
            (book . libre)
            (friend . ami)
            (hello . bonjour)
            (how . comment)
            (my . mon)
            (red . rouge)
            (you . tu)
            ]
          words))
#|
(define (subst new old tree)
  (define (replace elem)
    (if (eq? elem old) new elem))
  (define (sub tree)
    (cond [(null? tree) '()]
          [(pair? tree) (cons (replace (car tree))
                              (sub (cdr tree)))]
          [else (lookup tree)]))
  (sub tree))

(define (sublis subst-list tree)
  (define (lookup elem)
    (let1 subst (assoc elem subst-list)
      (if subst (cdr subst) elem)))
  (define (sub tree)
    (cond [(null? tree) '()]
          [(pair? tree) (cons (lookup (car tree))
                              (sub (cdr tree)))]
          [else (lookup tree)]))
  (sub tree))
|#
#?=(english->french '(hello my friend - how are you today ?))

;;; Exercise 3.5 [h] - Twenty Questions
(define (guesser data)
  #|ユーザの回答を記憶し、続くguessに利用
  |#
  )


;;
(define (mylength li)
  (fold (lambda (e a) (+ a 1)) 0 li))

(define (cl:reduce lispproc lis _initial-value initial-value)
;; 本来ちゃんとキーワードを読むべし
  (fold (lambda (e a) (lispproc a e)) initial-value lis))

(define (mylength* li)
  (cl:reduce (lambda (a e) (+ a 1)) li :initial-value 0))

#?=(mylength '(1 2 3))
#?=(mylength* '(1 2 3))
