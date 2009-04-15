(use srfi-1)
(require "./cl-emu")
(require "./debug")

(define-macro (debug-print x)
  (let ((vs (gensym)) (ep (gensym)))
	`(with-output-to-port
		 (current-error-port)
	   (lambda ()
		 (format #t "#?= ~a\n" ',x)
		 (receive ,vs ,x
		   ;;(string-append "DEBUG> " (x->string x) " => ~a\n")
		   (for-each (lambda (,vs)
					   (format #t " => ~a\n" ,vs))
					 ,vs)
		   (apply values ,vs))))))

;; p.657
(define *grammar* "The grammar used by GENERATE.");defvar

;; p.658
(define (make-rule lhs rhs) (list lhs '-> rhs))
(define (rule-lhs rule) (first rule))
(define (rule-rhs rule) (third rule))

(define (make-parse tree rem) (list 'parse tree rem))
(define (parse? x) (and (pair? x) (eq? 'parse (car x))))
(define (parse-tree parse) (second parse))
(define (parse-rem parse) (third parse))
(define (parse-lhs parse) (tree-lhs (parse-tree parse)))

(define (new-tree cat rhs) (cons cat rhs))
(define (tree-lhs tree) (car tree))
(define (tree-rhs tree) (cdr tree))

(define (lexical-rules word)
  "Return a list of rules with word on the right-hand side."
;  (find-all word *grammar* :key rule-rhs :test equal?))
  (filter (lambda (rule) (equal? word (rule-rhs rule))) *grammar*))

(define (rules-starting-with cat)
  "Return a list of rules where cat starts the rhs."
;  (find-all cat *grammar* :key (lambda (rule) (first-or-nil (rule-rhs rule)))))
  (filter (lambda (rule) (eq? cat (first-or-nil (rule-rhs rule))))
          *grammar*))

(define (first-or-nil x)
  "The first element of x if it is a list; else nil/#f."
  (if (pair? x) (car x) #f))

(define (parser words)
  "Return all complete parses of a list of words."
  (map parse-tree (complete-parses (parse words))))

(define (complete-parses parses)
  "Those parses that are complete (have no remainder)."
;  (filter null? parses :key parse-rem))
  (filter (lambda (item) (null? (parse-rem item))) parses))

;; p.659
(define (parse words)
  "Bottom-up parse, returning all parses of any prefix of words."
  (dbg :parse "(parse words:~a)" words)
  (if (null? words)
      '()
      (cl:mapcan (lambda (rule)
                (extend-parse (rule-lhs rule)
                              (list (car words))
                              (cdr words)
                              '()))
              (lexical-rules (car words)))))

(define (extend-parse lhs rhs rem needed)
  "Look for the categories needed to complete the parse."
  (dbg :parse "(extend-parse lhs:~a rhs:~a rem:~a needed:~a)" lhs rhs rem needed)
  (if (null? needed)
      ; If nothing needed, return parse and upward extensions
;     (let1 parse (make-parse :tree (new-tree lhs rhs) :rem rem)
      (let1 parse (make-parse (new-tree lhs rhs) rem)
        (cons parse
              (cl:mapcan (lambda (rule)
                        ;;(format #t "## rule:~a, parse:~a, rem:~a\n" rule parse rem)
                        (extend-parse (rule-lhs rule)
                                      (list (parse-tree parse))
                                      rem
                                      (cdr (rule-rhs rule))))
                      (rules-starting-with lhs))))
      ;; otherwise try to extend rightward
      (cl:mapcan (lambda (p)
                ;;(format #t "%% p:~a, needed:~a, lhs:~a, rhs:~a\n" p needed lhs rhs)
                (if (eq? (parse-lhs p) (car needed))
                    (extend-parse lhs (append1 rhs (parse-tree p))
                                  (parse-rem p) (cl:cdr needed))
                    '()))
              (parse rem))))

(define (append1 items item)
  "Add item to end of list of items."
  (append items (list item)))

;(debug :parse)