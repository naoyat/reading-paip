;(require "./cl-emu")
(require "./debug")
(require "./ch05-2")
;(define (rule-pattern rule) (first rule))
;(define (rule-responses rule) (rest rule))
(define rule-pattern car)
(define rule-responses cdr)

#;(((?* ?x) I want (?* ?y))
 (What would it mean if you got ?y)
 (Why do you want ?y)
 (Suppose you got ?y soon))

(define *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))
    ))

(define-macro (loop . body)
  (let1 loopname (gensym)
    `(let ,loopname () ,@body (,loopname))))

(define (eliza)
  (let loop ()
    (display "eliza> ") (flush)
    ;;(write (flatten #?=(use-eliza-rules (read))) ); :pretty #t)
    (let1 exp (read)
      (cond [(eof-object? exp) 'quit]
            [(atom? exp) (loop)]
            [else (let1 ans (use-eliza-rules exp)
                    (when ans (display (flatten ans)))
                    (newline)
                    (loop))]
            ))))

(define (use-eliza-rules input)
  (dbg :eliza "% (use-eliza-rules input:~a)" input)
  (any (lambda (rule)
         (dbg :eliza "%% (rule:~a) " rule)
         (dbg :eliza "  => (pat-match ~a input)" (rule-pattern rule))
         (let1 result (pat-match (rule-pattern rule) input)
           (dbg :eliza "  result: ~a" result)
           (if (not (eq? result fail))
               (sublis (switch-viewpoint result)
                       (random-elt (rule-responses rule)))
               #f)))
       *eliza-rules*))

(define (switch-viewpoint words)
  (dbg :eliza "% (switch-viewpoint words:~a)" words)
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

(define (flatten the-list)
  (mappend mklist the-list))

(define (mklist x)
  (if (pair? x) x (list x)))

(define (mappend fn the-list)
  (append-map fn the-list))

#;(define (random-elt choices)
  (elt choices (random (length choices))))
(define (random-elt choices)
  "Choose an element from a list at random."
  (cl:elt choices (cl:random (length choices))))

;(debug :eliza)
;(eliza)