(require "./cl-emu")
(require "./ch05-4")
(require "./ch06-1")

(define (pat-match pattern input . args)
  (let-optionals* args ((bindings no-bindings))
;	(format #t "pattern:~a input:~a bindings:~a\n" pattern input bindings)
    (cond [(eq? bindings fail) fail]
          [(variable? pattern)
           (match-variable pattern input bindings)]
          [(eqv? pattern input) bindings]
          [(segment-pattern? pattern)
           (segment-matcher pattern input bindings)]
          [(single-pattern? pattern)
           (single-matcher pattern input bindings)]
          [(and (pair? pattern) (pair? input))
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input)
                                 bindings))]
          [else fail])))

(define fail '())

(define no-bindings '((#t . #t)))

(define (variable? x)
  ;;(and (symbol? x) (equal? (char (symbol-name x) 0) #\?))
  (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?)))

(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
  (assoc var bindings))

(define (binding-val binding)
  ;; Get the value part of a single binding
  (cdr binding))

(define (make-binding var val)
  (cons var val))

(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (binding-val (get-binding var bindings)))

(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (cons (make-binding var val)
        (if (eq? bindings no-bindings)
            '()
            bindings)))

(define (match-variable var input bindings)
  (let1 binding (get-binding var bindings)
;	(format #t "(match-variable: input:~a, var:~a, binding:~a)\n" input var binding)
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

(define %ht (make-hash-table 'equal?))
(define (%get sym key)
  (hash-table-get %ht (cons sym key) #f));;(if #f #f)))
(define (%set! sym key val)
  (hash-table-put! %ht (cons sym key) val)
  val)
(define (funcall fn . args)
  (let1 proc (cond [(procedure? fn) fn]
                   [(symbol? fn)
                    (global-variable-ref (current-module) fn)]
                   [else #f])
    (when fn (apply proc args))))

(%set! '?is 'single-match 'match-is)
(%set! '?or 'single-match 'match-or)
(%set! '?and 'single-match 'match-and)
(%set! '?not 'single-match 'match-not)
(%set! '?* 'segment-match 'segment-match)
(%set! '?+ 'segment-match 'segment-match+)
(%set! '?? 'segment-match 'segment-match?)
(%set! '?if 'segment-match 'match-if)

(define (segment-pattern? pattern)
  (and (pair? pattern) (pair? (first pattern))
       (symbol? (first (first pattern)))
       (segment-match-fn (first (first pattern))) ))

(define (single-pattern? pattern)
  (and (pair? pattern)
       ;(format #t "(single-pattern? (~a ...)\n" (first pattern))
       (single-match-fn (first pattern))))

(define (segment-matcher pattern input bindings)
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(define (single-matcher pattern input bindings)
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(define (segment-match-fn x)
  (if (symbol? x) (%get x 'segment-match) #f))

(define (single-match-fn x)
  (if (symbol? x) (%get x 'single-match) #f))

;;
(define (match-is var&pred input bindings)
  (let* ([var (first var&pred)]
         [pred (second var&pred)]
         [new-bindings (pat-match var input bindings)])
    (if (or (eq? new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(define (match-and patterns input bindings)
  (cond [(eq? bindings fail) fail]
        [(null? patterns) bindings]
        [else (match-and (rest patterns) input
                         (pat-match (first patterns) input bindings))]))

(define (match-or patterns input bindings)
  (if (null? patterns)
      fail
      (let1 new-bindings (pat-match (first patterns) input bindings)
        (if (eq? new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(define (match-not patterns input bindings)
  (if (cl:thru (match-or patterns input bindings))
      fail
      bindings))

;;
(define (segment-match pattern input bindings . args)
  (let-optionals* args ((start 0))
      (let ([var (second (first pattern))]
            [pat (rest pattern)])
;        (format #t "(segment-match pattern:(var:~a pat:~a) input:~a)\n" var pat input)
        (if (null? pat)
            (match-variable var input bindings)
            (let1 pos (first-match-pos (first pat) input start)
              (if pos
                  (let1 b2 (pat-match pat (subseq input pos)
                                      (match-variable var (subseq input 0 pos) bindings))
                    (if (eq? b2 fail)
                        (segment-match pattern input bindings (+ pos 1))
                        b2))
                  fail)
              )))))

(define (first-match-pos pat1 input start)
  (cond [(and (cl:atom pat1) (not (variable? pat1)))
         (cl:position pat1 input :start start :test equal?)]
        [(< start (length input)) start]
        [else #f]))
#|
#?=(pat-match '(a (?* ?x) d) '(a b c d))
#?=(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))
#?=(pat-match '(a (?* ?x) (?* ?y) ?x ?y)
              '(a b c d (b c) (d)))
|#
(define (segment-match+ pattern input bindings)
  (segment-match pattern input bindings 1))

(define (segment-match? pattern input bindings)
  (let ([var (second (first pattern))]
        [pat (rest pattern)])
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(define-macro (progv symbols values . body)
  #;(format #t "(progv s:~a v:~a b:~a)\n" symbols values body)
  `(let ([module (current-module)]
         [env (interaction-environment)])
     (let loop ((vars ,symbols) (vals ,values))
       (if (null? vars)
           (begin
             ,@body);;(eval ,body env)
           (begin
             (eval (list 'define (car vars) (car vals)) env)
             (loop (cdr vars) (cdr vals))) ))))

(define (match-if pattern input bindings)
  (and (progv (map car bindings)
              (map cdr bindings)
              (eval (second (first pattern))
                    (interaction-environment) ))
       (pat-match (rest pattern) input bindings)))
#|
#?=(pat-match '(?x ?op ?y is ?z (?if (eqv? (?op ?x ?y) ?z)))
              '(3 + 4 is 7))
#?=(pat-match '(?x ?op ?y (?if (?op ?x ?y)))
              '(3 > 4))
|#

(define (pat-match-abbrev symbol expansion)
  (%set! symbol 'expand-pat-match-abbrev
         (expand-pat-match-abbrev expansion)))

(define (expand-pat-match-abbrev pat)
  #;(format #t "(expand-pat-match-abbrev ~a)\n" pat)
;;(cond [(and (symbol? pat) (%get pat 'expand-pat-match-abbrev))]
  (cond [(null? pat) '()] ;**
        [(and (symbol? pat) (%get pat 'expand-pat-match-abbrev))]
        [(cl:atom pat) pat]
        [else (cons (expand-pat-match-abbrev (first pat))
                    (expand-pat-match-abbrev (rest pat)))]))
#|
#?=(pat-match-abbrev '?x* '(?* ?x))
#?=(pat-match-abbrev '?y* '(?* ?y))
(define axyd #?=(expand-pat-match-abbrev '(a ?x* ?y* d)))
#?=(pat-match axyd '(a b c d))
|#

;;6.3
(define (rule-based-translator input rules . args)
  (let-keywords* args ((matcher pat-match)
                       (rule-if first)
                       (rule-then rest)
                       (action sublis))
    (any (lambda (rule)
		   rule
           (let1 result (matcher (rule-if rule) input)
             (if (not (eq? result fail))
                 (action result (rule-then rule))
                 #f;'()
                 )))
         rules)))

;;;
(define (use-eliza-rules input)
  (rule-based-translator input *eliza-rules*
                         :action (lambda (bindings responses)
                                   (sublis (switch-viewpoint bindings)
                                           (random-elt responses)))))

(define *eliza-rules*
  '(((?x* hello ?y*)
     (How do you do.  Please state your problem.))
    ((?x* computer ?y*)
     (Do computers worry you?) (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))
    ((?x* name ?y*)
     (I am not interested in names))
    ((?x* sorry ?y*)
     (Please don't apologize) (Apologies are not necessary))
    ((?x* I remember ?y*)
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember?) (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y)
     (What is the connection between me and ?y))
    ((?x do you remember ?y*)
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now)
     (What about ?y) (You mentioned ?y))
    ((?x* if ?y*)
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))

    ((?x* I dreamt ?y*)
     (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt ?y before?))
    ((?x* dream about ?y*)
     (How do you feel about ?y in reality?))
    ((?x* dream ?y*)
     (What does this dream suggest to you?) (Do you dream often?)
     (What persons appear in your dreams?)
     (Don't you believe that dream has to do with your problem?))
    ((?x* my mother ?y*)
     (Who else in your family ?y) (Tell me more about your family))
    ((?x* my father ?y*)
     (Your father) (Does he influence you strongly?)
     (What else comes to mind when you think of your father?))

    ((?x* I want ?y*)
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    ((?x* I am glad ?y*)
     (How have I helped you to be ?y) (What makes you happy just now)
     (Can you explain why you are suddenly ?y))
    ((?x* I am sad ?y*)
     (I am sorry to hear you are depressed)
     (I'm sure it's not pleasant to be sad))
    ((?x* are like ?y*)
     (What resemblance do you see between ?x and ?y))
    ((?x* is like ?y*)
     (In what way is that ?x is like ?y)
     (What resemblance do you see?)
     (Could there really be some connection?) (How?))
    ((?x* alike ?y*)
     (In what way?) (What similarities are there?))
    ((?x* same ?y*)
     (What other connections do you see?))

    ((?x* I was ?y*)
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    ((?x* was I ?y*)
     (What if you were ?y ?) (Do you think you were ?y)
     (What would it mean if your were ?y))
    ((?x* I am ?y*)
     (In what way are you ?y) (Do you want to be ?y ?))
    ((?x* am I ?y*)
     (Do you believe you are ?y) (Would you want to be ?y)
     (You wish I would tell you you are ?y)
     (What would it mean if you were ?y))
    ((?x* am ?y*)
     (Why do you say "AM?") (I don't understand that))
    ((?x* are you ?y*)
     (Why are you interested in whether I am ?y or not?)
     (Would you prefer if I weren't ?y)
     (Perhaps I am ?y in your fantasies))
    ((?x* you are ?y*)
     (What makes you think I am ?y ?))
    
    ((?x* because ?y*)
     (Is that the real reason?) (What other reasons might there be?)
     (Does that reason seem to explain anything else?))
    ((?x* were you ?y*)
     (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
    ((?x* I can't ?y*)
     (Maybe you could ?y now) (What if you could ?y ?))
    ((?x* I feel ?y*)
     (Do you often feel ?y ?))
    ((?x* I felt ?y*)
     (What other feelings do you have?))
    ((?x* I ?y* you ?z*)
     (Perhaps in your fantasy we ?y each other))
    ((?x* why don't you ?y*)
     (Should you ?y yourself?)
     (Do you believe I don't ?y) (Perhaps I will ?y in good time))
    ((?x* yes ?y*)
     (You seem quite positive) (You are sure) (I understand))
    ((?x* no ?y*)
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))

    ((?x* someone ?y*)
     (Can you be more specific?))
    ((?x* everyone ?y*)
     (surely not everyone) (Can you think of anyone in particular?)
     (WHo for example?) (You are thinking of a special person))
    ((?x* always ?y*)
     (Can you think of a specific example) (When?)
     (What incident are you thinking of?) (Really-- always))
    ((?x* what ?y*)
     (Why do you ask?) (Does that question interest you?)
     (What is it you really want to know?) (What do you think?)
     (What comes to your mind when you ask that?))
    ((?x* perhaps ?y*)
     (You do not seem quite certain))
    ((?x* are ?y*)
     (Did you think they might not be ?y)
     (Possibly they are ?y))
    ((?x*)
     (Very interesting) (I am not sure I understand you fully)
     (What does that suggest to you?) (Please continue) (Go on)
     (Do you feel strongly about discussing such things?))
    ))

(define (interactive-interpreter prompt transformer)
  (let loop ()
    (handler-case
     (begin
       (if (string? prompt)
           (display prompt)
           (prompt))
       (print (transformer (read)))
       (loop)
       )
     (error (condition)
            (format #t ";; Error ~a ignored, back to top level.\n" condition)
            ))))

;(interactive-interpreter (prompt-generator) (compose flatten use-eliza-rules))
