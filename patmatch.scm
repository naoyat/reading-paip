(require "./cl-emu")
;(require "./ch05-4")
;(require "./ch06-1")

(define (variable? x)
  "Is x a variable (a symbol beginning with `?')?"
;  (and (symbol? x) (equal? (elt (symbol-name x) 0) #\?)))
  (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?)))

(define (pat-match pattern input . args)
  "Match pattern against input in the context of the bindings"
  (let-optionals* args ((bindings no-bindings))
	(let1 rv (cond [(eq? bindings fail) fail]
				   [(variable? pattern)
					(match-variable pattern input bindings)]
				   [(eqv? pattern input) bindings]
				   [(segment-pattern? pattern)
					(segment-matcher pattern input bindings)]
				   [(single-pattern? pattern)                 ; ***
					(single-matcher pattern input bindings)]   ; ***
				   [(and (pair? pattern) (pair? input))
					(pat-match (cdr pattern) (cdr input)
							   (pat-match (car pattern) (car input) 
										  bindings))]
				   [else fail])
	  (dbg :trace "(pat-match pattern:~a input:~a [bindings:~a]) => ~a"
		   pattern input bindings rv)
	  rv)))

(%set! '?is  'single-match 'match-is)
(%set! '?or  'single-match 'match-or)
(%set! '?and 'single-match 'match-and)
(%set! '?not 'single-match 'match-not)

(%set! '?*  'segment-match 'segment-match)
(%set! '?+  'segment-match 'segment-match+)
(%set! '??  'segment-match 'segment-match?)
(%set! '?if 'segment-match 'match-if)

(define (segment-pattern? pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (let1 rv (and (pair? pattern) (pair? (car pattern))
				(symbol? (caar pattern))
				(segment-match-fn (caar pattern)))
	(dbg :trace "(segment-pattern? pattern:~a) => ~a" pattern rv)
	rv))

(define (single-pattern? pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (let1 rv (and (pair? pattern)
				(single-match-fn (car pattern)))
	(dbg :trace "(single-pattern? pattern:~a) => ~a" pattern rv)
	rv))

(define (segment-matcher pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (let1 rv (funcall (segment-match-fn (caar pattern))
					pattern input bindings)
	(dbg :trace "(segment-matcher pattern:~a input:~a bindings:~a) => ~a"
		 pattern input bindings  rv)
	rv))

(define (single-matcher pattern input bindings)
  "Call the right function for this kind of single pattern."
  (let1 rv (funcall (single-match-fn (car pattern))
					(cdr pattern) input bindings)
	(dbg :trace "(single-matcher pattern:~a input:~a bindings:~a) => ~a"
		 pattern input bindings  rv)
	rv))

(define (segment-match-fn x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (let1 rv (cl:thru
			(cl:when (symbol? x) (%get x 'segment-match)))
	(dbg :trace "(segment-match-fn x:~a) => ~a" x rv)
	rv))

(define (single-match-fn x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (let1 rv (cl:thru
			(cl:when (symbol? x) (%get x 'single-match)))
	(dbg :trace "(single-match-fn x:~a) => ~a" x rv)
	rv))

(define (match-is var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let1 rv (let* ([var (first var&pred)]
				  [pred (second var&pred)]
				  [new-bindings (pat-match var input bindings)])
			 (if (or (eq? new-bindings fail)
					 (not (funcall pred input)))
				 fail
				 new-bindings))
	(dbg :trace "(match-is var-and-pred:~a input:~a bindings:~a) => ~a"
		 var-and-pred input bindings  rv)
	rv))

(define (match-and patterns input bindings)
  "Succeed if all the patterns match the input."
  (let1 rv (cond [(eq? bindings fail) fail]
				 [(null? patterns) bindings]
				 [else (match-and (rest patterns) input
								  (pat-match (first patterns) input bindings))])
	(dbg :trace "(match-and patterns:~a input:~a bindings:~a) => ~a"
		 patterns input bindings  rv)
	rv))

(define (match-or patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (let1 rv (if (null? patterns)
			   fail
			   (let1 new-bindings (pat-match (first patterns) input bindings)
				 (if (eq? new-bindings fail)
					 (match-or (rest patterns) input bindings)
					 new-bindings)))
	(dbg :trace "(match-or patterns:~a input:~a bindings:~a) => ~a"
		 patterns input bindings  rv)
	rv))

(define (match-not patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (let1 rv (if (cl:thru (match-or patterns input bindings))
			   fail
			   bindings)
	(dbg :trace "(match-not patterns:~a input:~a bindings:~a) => ~a"
		 patterns input bindings  rv)
	rv))

(define (segment-match pattern input bindings . args)
  "Match the segment pattern ((?* var) . pat) against input."
  (let-optionals* args ((start 0))
	(let1 rv (let ([var (second (first pattern))]
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
					 )))
	  (dbg :trace "(segment-match pattern:~a input:~a bindings:~a [start:~d]) => ~a"
		   pattern input bindings start rv)
	  rv)))

(define (first-match-pos pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (let1 rv (cond [(and (cl:atom pat1) (not (variable? pat1)))
				  (cl:position pat1 input :start start :test equal?)]
;        [(< start (length input)) start]
				 [(<= start (length input)) start] ;*** fix, rjf 10/1/92 (was <)
				 [else #f])
	(dbg :trace "(first-match-pos pat1:~a input:~a start:~a) => ~a"
		 pat1 input start rv)
	rv))

(define (segment-match+ pattern input bindings)
  "Match one or more elements of input."
  (let1 rv (segment-match pattern input bindings 1)
	(dbg :trace "(segment-match+ pattern:~a input:~a bindings:~a) => ~a"
		 pattern input bindings rv)
	rv))

(define (segment-match? pattern input bindings)
  "Match zero or one element of input."
  (let1 rv (let ([var (second (first pattern))]
				 [pat (rest pattern)])
			 (or (pat-match (cons var pat) input bindings)
				 (pat-match pat input bindings)))
	(dbg :trace "(segment-match? pattern:~a input:~a bindings:~a) => ~a"
		 pattern input bindings rv)
	rv))
  
(define (match-if pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (let1 rv (and (progv (map car bindings)
					   (map cdr bindings)
			  ;(eval (second (first pattern))))
					   (eval (second (car pattern))
							 (interaction-environment) ))
				(pat-match (cdr pattern) input bindings))
	(dbg :trace "(match-if pattern:~a input:~a bindings:~a) => ~a"
		 pattern input bindings rv)
	rv))

(define (pat-match-abbrev symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (let1 rv (%set! symbol 'expand-pat-match-abbrev
				  (expand-pat-match-abbrev expansion))
	(dbg :trace "(pat-match-abbrev symbol:~a expansion:~a) => ~a"
		 symbol expansion rv)
	rv))

(define (expand-pat-match-abbrev pat)
  "Expand out all pattern matching abbreviations in pat."
  (let1 rv (cond [(null? pat) '()] ;**
				 [(and (symbol? pat) (%get pat 'expand-pat-match-abbrev))]
				 [(cl:atom pat) pat]
				 [else (cons (expand-pat-match-abbrev (car pat))
							 (expand-pat-match-abbrev (cdr pat)))])
	(dbg :trace "(expand-pat-match-abbrev pat:~a) => ~a" pat rv)
	rv))

(define (rule-based-translator input rules . args)
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (let-keywords* args ((matcher pat-match)
                       (rule-if first)
                       (rule-then rest)
                       (action cl:sublis))
	(let1 rv (any (lambda (rule)
					rule
					(let1 result (matcher (rule-if rule) input)
					  (if (not (eq? result fail))
						  (action result (rule-then rule))
						  #f;'()
						  )))
				  rules)
	  (dbg :trace "(rule-based-translator input:~a rules:~a // matcher:~a rule-if:~a rule-then:~a action:~a) => ~a"
		   input rules matcher rule-if rule-then action  rv)
	  rv)))

