(use srfi-1)
(use srfi-13) ;string-upcase

;;; l.139
(define (find-anywhere item tree)
  "Does item occur anywhere in tree?  If so, return it."
  ;(format #t "(find-anywhere item:~a in tree:~a)..\n" item tree)
  (let1 rv (cond [(eqv? item tree) tree]
				 [(cl:atom tree) #f]
				 [(null? tree) #f]
				 [else (or (find-anywhere item (first tree))
						   (find-anywhere item (rest tree)))]
				 #;[(find-anywhere item (first tree))]
				 #;[(find-anywhere item (rest tree))]
				 )
;	(dbg :trace "(find-anywhere item:~a tree:~a) => ~a" item tree rv)
	rv))

;;; l.149
(define (starts-with lis x)
  ;; "Is this a list whose first element is x?"
  ;(format #t "(starts-with lis:~a x:~a)\n" lis x)
  (let1 rv (and (pair? lis) (eqv? (car lis) x))
;	(dbg :trace "(starts-with lis:~a x:~a) => ~a" lis x rv)
	rv))

;;;; Auxiliary Functions

; l.155
(define (find-all item seq . args)
  (let-keywords* args ([key identity]
					   [test eqv?]
					   [test-not #f]) ;; wish to allow other keys...
;	(filter (lambda (el) (test item (key el))) seq)))
	(when test-not (set! test (lambda (x y) (not (test x y)))))
	(let1 rv (filter (lambda (e) (test item (key e))))
;	  (dbg :trace "(find-all item:~a seq:~a [key:~a test:~a]) => ~a" item seq key test  rv)
	  rv)))

; l.165
(define (partition-if pred lis)
  ;; Return 2 values: elements of list that satisfy pred.
  ;(format #t "(partition-if pred:~a list:~a)\n" pred lis)
  (let1 rv (let ([yes-list '()]
				 [no-list '()])
			 (dolist (item lis)
			   (if (funcall pred item)
				   (cl:push item yes-list)
				   (cl:push item no-list)))
			 (values (reverse! yes-list) (reverse! no-list)))
;	(dbg :trace "(partition-if pred:~a lis:~a) => ~a" pred lis rv)
	rv))

;;; ==============================

(define (mappend fn the-list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  ;(apply append (map fn the-list))
  (let1 rv (append-map fn the-list)
;	(dbg :trace "(mappend fn:~a list:~a) => ~a" fn the-list rv)
	rv))

(define (mklist x)
  "If x is a list return it, otherwise return the list of x"
  (let1 rv (if (list? x) x (list x))
;	(dbg :trace "(mklist x:~a) => ~a" x rv)
	rv))

(define (flatten exp)
  "Get rid of imbedded lists (to one level only)."
  (let1 rv (mappend mklist exp)
;	(dbg :trace "(flatten exp:~a) => ~a" exp rv)
	rv))

(define (random-elt choices)
  "Pick a random element out of a sequence."
  (let1 rv (cl:elt choices (cl:random (length choices)))
;	(dbg :trace "(random-elt choices:~a) => ~a" choices rv)
	rv))

;;; ==============================

; l.242
(define member-equal member)

;;;; The Debugging Output Facility:

(define *dbg-ids* '())
(define *debug-io* (current-output-port))
(define *trace-indent* 0)

(define (dbg id format-string . args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (apply format *debug-io* format-string args)
    (newline *debug-io*)))

(define (debug . ids)
  "Start dbg output on the given ids."
  (set! *dbg-ids* (lset-union eq? ids *dbg-ids*))
  (set! *trace-indent* 0))
(define (undebug . ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (set! *dbg-ids* (if (null? ids) '()
                      (lset-difference eq? *dbg-ids* ids))))

;;; ==============================

(define (dbg-indent id indent format-string . args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (dotimes (i indent) (display "  " *debug-io*))
    (apply format *debug-io* format-string args)
    (newline *debug-io*) ))

(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define (trace-in fn . args)
  (dbg-indent :trace *trace-indent* "~d: ~a" *trace-indent* (cons (symbol-upcase fn) args))
  (inc! *trace-indent*))

(define (trace-out fn rv)
  (dec! *trace-indent*)
  (dbg-indent :trace *trace-indent* "~d: ~a returned ~a" *trace-indent* (symbol-upcase fn) rv)
  )

;;;; PATTERN MATCHING FACILITY

(define fail '())
(define no-bindings '((#t . #t)))

(define (pat-match pattern input . args)
  "Match pattern against input in the context of the bindings"
  (let-optionals* args ((bindings no-bindings))
	(let1 rv (cond [(eq? bindings fail) fail]
				   [(variable? pattern) (match-variable pattern input bindings)]
				   [(eqv? pattern input) bindings]
				   [(and (pair? pattern) (pair? input))
					(pat-match (cdr pattern) (cdr input)
							   (pat-match (car pattern) (car input) bindings))]
				   [else fail])
;	  (dbg :trace "(pat-match pattern:~a input:~a [bindings:~a]) => ~a" pattern input bindings rv)
	  rv)))

(define (match-variable var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let1 binding (get-binding var bindings)
;	(format #t "(match-variable: input:~a, var:~a, binding:~a)\n" input var binding)
    (let1 rv (cond [(not binding) (extend-bindings var input bindings)]
				   [(equal? input (binding-val binding)) bindings]
				   [else fail])
;	  (dbg :trace "(match-variable var:~a input:~a bindings:~a // binding:~a) => ~a"
;		   var input bindings
;		   binding rv)
	  rv)))

(define (make-binding var val)
  (let1 rv (cons var val)
;;%	(dbg :trace "(make-binding var:~a val:~a) => ~a" var val rv)
	rv))

(define (binding-var binding)
  ;;"Get the variable part of a single binding."
;  (let1 rv (cl:car binding)
  (let1 rv (car binding)
;;%	(dbg :trace "(binding-var binding~a) => ~a" binding rv)
	rv))

(define (binding-val binding)
  ;; Get the value part of a single binding
;  (let1 rv (cl:thru (cl:cdr binding))
  (let1 rv (cdr binding)
;;%	(dbg :trace "(binding-val binding:~a) => ~a" binding rv)
	rv))

(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
;  (trace-in 'get-binding var bindings)
  (let1 rv (assoc var bindings)
;	(dbg :trace "(get-binding var:~a bindings:~a) => ~a" var bindings rv)
;	(trace-out 'get-binding rv)
	rv))

(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (let1 rv (binding-val (get-binding var bindings))
;	(dbg :trace "(lookup var:~a bindings:~a) => ~a" var bindings rv)
	rv))

(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (let1 rv (cons (make-binding var val)
		;; Once we add a "real" binding,
		;; we can get rid of the dummy no-bindings
				 (if (eq? bindings no-bindings)
					 '()
					 bindings))
;;%	(dbg :trace "(extend-bindings var:~a val:~a bindings:~a) => ~a"
;;%		 var val bindings rv)
	rv))

(define (variable? x)
  "Is x a variable (a symbol beginning with '?')?"
;;(and (symbol? x) (equal? (char (symbol-name x) 0) #\?)))
  (let1 rv (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?))
;	(dbg :trace "(variable? x:~a) => ~a" x rv)
	rv))

;;; ==============================

; l.451
(define (reuse-cons x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (let1 rv (if (and (eqv? x (car x-y)) (eqv? y (cdr x-y)))
			   x-y
			   (cons x y))
;;%	(dbg :trace "(reuse-cons x:~a y:~a x-y:~a) => ~a" x y x-y rv)
	rv))

;;; ==============================

(define (length=1? x)
  "Is x a list of length 1?"
  (let1 rv (and (pair? x) (null? (cdr x)))
;	(dbg :trace "(length=1? x:~a) => ~a" x rv)
	rv))

(define (rest3 lis) (cdddr lis)); "The rest of a list after the first THREE elements."
