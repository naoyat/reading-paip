(use srfi-1)
#|
;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (cl:every symbol? variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (cl:every side-effect-free? (list .,variables))
	(begin .,body)
	(list 'let
	 ,`(list ,@(map (lambda (tmp var)
			       `(list ',tmp ,var))
			   temps variables))
	 (let ,(map (lambda (var tmp) `(,var ',tmp))
		       variables temps)
	   .,body)))))

  (define (side-effect-free? exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

  (define (rest2 x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))
|#
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
	(dbg :trace "(find-anywhere item:~a tree:~a) => ~a" item tree rv)
	rv))

;;; l.149
(define (starts-with lis x)
  ;; "Is this a list whose first element is x?"
  ;(format #t "(starts-with lis:~a x:~a)\n" lis x)
  (let1 rv (and (pair? lis) (eqv? (car lis) x))
	(dbg :trace "(starts-with lis:~a x:~a) => ~a" lis x rv)
	rv))

;;;; Auxiliary Functions
#|
(setf (symbol-function 'find-all-if) remove-if-not)
|#

; l.155
(define (find-all item seq . args)
  (let-keywords* args ([key identity]
					   [test eqv?]
					   [test-not #f]) ;; wish to allow other keys...
;	(filter (lambda (el) (test item (key el))) seq)))
	(when test-not (set! test (lambda (x y) (not (test x y)))))
	(let1 rv (filter (lambda (e) (test item (key e))))
	  (dbg :trace "(find-all item:~a seq:~a [key:~a test:~a]) => ~a" item seq key test  rv)
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
	(dbg :trace "(partition-if pred:~a lis:~a) => ~a" pred lis rv)
	rv))
#|
(define (maybe-add op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null? exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

(define (seq-ref seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (list? ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))

(define (maybe-set-fill-pointer array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (let1 rv (if (and (array? array)
					(array-has-fill-pointer? array))
			   (setf (fill-pointer array) 
					 (max (fill-pointer array) new-length)))
	(dbg :trace "(maybe-set-fill-pointer array:~a new-length:~a) => ~a" array new-length rv)
	rv))

;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(define (symbol . args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(define (new-symbol . args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(define (last1 lis)
  "Return the last element (not last cons cell) of list"
  (first (last lis)))
|#

;;; ==============================

(define (mappend fn the-list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  ;(apply append (map fn the-list))
  (let1 rv (append-map fn the-list)
	(dbg :trace "(mappend fn:~a list:~a) => ~a" fn the-list rv)
	rv))

(define (mklist x)
  "If x is a list return it, otherwise return the list of x"
  (let1 rv (if (list? x) x (list x))
	(dbg :trace "(mklist x:~a) => ~a" x rv)
	rv))

(define (flatten exp)
  "Get rid of imbedded lists (to one level only)."
  (let1 rv (mappend mklist exp)
	(dbg :trace "(flatten exp:~a) => ~a" exp rv)
	rv))

(define (random-elt choices)
  "Pick a random element out of a sequence."
  (cl:elt choices (cl:random (length choices))))

;;; ==============================

; l.242
(define member-equal member)

;;; ==============================
#|
(define (compose . functions)
  (lambda (x)
	(cl:reduce funcall functions :from-end t :initial-value x)))
|#
;;;; The Debugging Output Facility:

(define *dbg-ids* '())
(define *debug-io* (current-output-port))

(define (dbg id format-string . args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (apply format *debug-io* format-string args)
    (newline *debug-io*)))

(define (debug . ids)
  "Start dbg output on the given ids."
  (set! *dbg-ids* (lset-union eq? ids *dbg-ids*)))

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

;;;; PATTERN MATCHING FACILITY

(define fail '())
(define no-bindings '((#t . #t)))

(define (pat-match pattern input . args)
  "Match pattern against input in the context of the bindings"
  (let-optionals* args ((bindings no-bindings))
    (cond [(eq? bindings fail) fail]
          [(variable? pattern) (match-variable pattern input bindings)]
          [(eqv? pattern input) bindings]
          [(and (pair? pattern) (pair? input))
           (pat-match (cdr pattern) (cdr input)
                      (pat-match (car pattern) (car input) bindings))]
          [else fail])))

(define (match-variable var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let1 binding (get-binding var bindings)
;	(format #t "(match-variable: input:~a, var:~a, binding:~a)\n" input var binding)
    (cond [(not binding) (extend-bindings var input bindings)]
          [(equal? input (binding-val binding)) bindings]
          [else fail])))

(define (make-binding var val) (cons var val))

(define (binding-var binding)
  ;;"Get the variable part of a single binding."
  (cl:car binding))

(define (binding-val binding)
  ;; Get the value part of a single binding
  (cl:thru (cl:cdr binding)))

(define (get-binding var bindings)
  ;; Find a (variable . value) pair in a binding list.
  (assoc var bindings))

(define (lookup var bindings)
  ;; Get the value part (for var) from a binding list.
  (binding-val (get-binding var bindings)))

(define (extend-bindings var val bindings)
  ;; Add a (var . value) pair to a binding list.
  (cons (make-binding var val)
		;; Once we add a "real" binding,
		;; we can get rid of the dummy no-bindings
        (if (eq? bindings no-bindings)
            '()
            bindings)))

(define (variable? x)
  "Is x a variable (a symbol beginning with '?')?"
;;(and (symbol? x) (equal? (char (symbol-name x) 0) #\?)))
  (and (symbol? x) (equal? (string-ref (symbol->string x) 0) #\?)))

;;; ==============================
#|
;;;; The Memoization facility:

(define-macro (defun-memo fn args . body)
  "Define a memoized function."
  `(memoize (define (,fn ,args ,@body)))

(define (memo fn &key (key first) (test eqv?) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    (lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found? val
                (setf (gethash k table) (apply fn args))))))))

(define (memoize fn-name &key (key first) (test eqv?))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(define (clear-memoize fn-name)
  "Clear the hash table from a memo function."
  (let1 table (get fn-name 'memo)
    (when table (clrhash table))))

;;;; Delayed computation:

(defstruct delay value (computed? nil))

(define-macro (delay . body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value (lambda () . ,body)))

(define (force delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))

;;;; Defresource:

(define-macro (defresource name &key constructor (initial-copies 0)
                       (size (max initial-copies 10)))
  (let ((resource (symbol '* (symbol name '-resource*)))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(begin
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (define (,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (define (,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc ,deallocate (loop repeat ,initial-copies 
                                       collect (,allocate))))
       ',name)))

(define-macro (with-resource (var resource &optional protect) . body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (begin (setf ,var (,allocate)) ,@body)
             (unless (null? ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;;; Queues:

;;; A queue is a (last . contents) pair

(define (queue-contents q) (cdr q))

(define (make-queue)
  "Build a new queue, with no elements."
  (let ((q (cons cl:nil cl:nil)))
    (setf (car q) q)))

(define (enqueue item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item cl:nil)))
  q)

(define (dequeue q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null? (cdr q)) (setf (car q) q))
  q)

(define (front q) (car (queue-contents q)))

(define (empty-queue? q) (null? (queue-contents q)))

(define (queue-nconc q lis)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (cdar q) lis))))

;;;; Other:

(define (sort* seq pred &key key) 
  "Sort without altering the sequence"
  (cl:sort (copy-seq seq) pred :key key))
|#

; l.451
(define (reuse-cons x y x-y)
  "Return (cons x y), or just x-y if it is equal to (cons x y)."
  (if (and (eqv? x (car x-y)) (eqv? y (cdr x-y)))
	  x-y
	  (cons x y)))

;;; ==============================

(define (length=1? x)
  "Is x a list of length 1?"
  (and (pair? x) (null? (cdr x))))

(define (rest3 lis) (cdddr lis)); "The rest of a list after the first THREE elements."

;;; ==============================
#|
(define (unique-find-if-anywhere predicate tree
								 &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (cl:atom tree)
      (if (funcall predicate tree)
          (cl:adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (car tree)
        (unique-find-if-anywhere predicate (cdr tree)
                                 found-so-far))))

(define (find-if-anywhere predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (cl:atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (car tree))
          (find-if-anywhere predicate (cdr tree)))))

;;; ==============================

(define-macro (define-enumerated-type type . elements)
  "Represent an enumerated type with integers 0-n."
  `(begin
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (define (,(symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (define (,(symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(loop for element in elements
             for i from 0
             collect `(defconstant ,element ,i))))

;;; ==============================

(define (not-null? x) (not (null? x)))

(define (first-or-nil x)
  "The first element of x if it is a list; else nil."
  (if (pair? x) (car x) '()))

(define (first-or-self x)
  "The first element of x, if it is a list; else x itself."
  (if (pair? x) (car x) x))

;;; ==============================

;;;; CLtL2 and ANSI CL Compatibility

(define-macro (defmethod name args . body)
  `(define (',name ',args ,@body)))

(define (map-into result-sequence function . sequences)
  "Destructively set elements of RESULT-SEQUENCE to the results
  of applying FUNCTION to respective elements of SEQUENCES."
  (let ((arglist (make-list (length sequences)))
        (n (if (list? result-sequence)
               most-positive-fixnum
               (array-dimension result-sequence 0))))
    ;; arglist is made into a list of args for each call
    ;; n is the length of the longest vector
    (when sequences
      (set! n (min n (loop for seq in sequences
                           minimize (length seq)))))
    ;; Define some shared functions:
    (flet
      ((do-one-call (i)
         (loop for seq on sequences
               for arg on arglist
               do (if (list? (first seq))
                      (setf (first arg)
                            (pop (first seq)))
                      (setf (first arg)
                            (aref (first seq) i))))
         (apply function arglist))
       (do-result (i)
         (if (and (vector? result-sequence)
                  (array-has-fill-pointer? result-sequence))
             (setf (fill-pointer result-sequence) 
                   (max i (fill-pointer result-sequence))))))
      (declare (inline do-one-call))
      ;; Decide if the result is a list or vector,
      ;; and loop through each element
      (if (list? result-sequence)
          (loop for i from 0 to (- n 1)
                for r on result-sequence
                do (setf (first r)
                         (do-one-call i))
                finally (do-result i))
          (loop for i from 0 to (- n 1)
                do (setf (aref result-sequence i)
                         (do-one-call i))
                finally (do-result i))))
      result-sequence))

)

(define (complement fn)
  "If FN returns y, then (complement FN) returns (not y)."
  (lambda (&rest args) (not (apply fn args))))

(define-macro (with-compilation-unit options . body)
  "Do the body, but delay compiler warnings until the end."
  ;; That way, undefined function warnings that are really
  ;; just forward references will not be printed at all.
  ;; This is defined in Common Lisp the Language, 2nd ed.
;  (declare (ignore options))
  `(,(read-time-case
       #+Lispm 'compiler:compiler-warnings-context-bind
       #+Lucid 'with-deferred-warnings
               'begin)
    .,@body))

;;;; Reduce

(when #f ;; Change this to T if you need REDUCE with :key keyword.

(define (reduce* fn seq from-end start end key init init-p)
  (funcall (if (list? seq) reduce-list reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

(define (reduce function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

(define (reduce-vect fn seq from-end start end key init init-p)
  (if (null? end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
           (funcall-if key (aref seq start))))
    (0 (if init? init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (funcall-if key (aref seq start))
                         (funcall-if key (aref seq (+ start 1)))))))
             (loop for i from (+ start (if init? 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 2)))
                         (funcall-if key (aref seq (- end 1)))))))
             (loop for i from (- end (if init? 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (funcall-if key (aref seq i))
                              result)))
             result)))))

(define (reduce-list fn seq from-end start end key init init-p)
  (if (null? end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null? seq) (eqv? start end))
         (if init? init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) #t start end
                      key init init?))
        ((null? (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (pop seq)))
                       (funcall
                         fn
                         (funcall-if key (pop seq))
                         (funcall-if key (pop seq))))))
             (if end
                 (loop repeat (- end (if init? 1 2)) while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq)))))
                 (loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq))))))
             result))))
)
|#