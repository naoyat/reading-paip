(require "./ch20-1") ;; ?-
(require "./ch20-2")

(use gauche.test)
(test-start "§ 20.3 - DCG rules")

;(memoize lexical-rules)
;(memoize rules-starting-with)
;(memoize parse :test eq?)

;(define-macro (test-set=* name expected expr)
;  `(test* ,name ,expected ,expr
;		  (lambda (a b) (lset= equal? a b))))

;(<- (funcall (lambda (?x) ?body) ?x ?body))
(require "./ch20-688-rules-dcg")
(newline)

(print (?- (S ?sem (the boys kiss a girl) ())))
(print (?- (S ?sem (the girls kissed the girls) ())))
(print (?- (S ?sem (Terry kissed the girl) ())))
(print (?- (S ?sem (the girls kissed the boys) ())))
(print (?- (S ?sem (Terry kissed a girls) ())))
(print (?- (S ?sem (Terry sleeps Jean) ())))

(test-end)

