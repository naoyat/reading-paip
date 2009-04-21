(require "./prolog")

(define-macro (?- . goals)
;  `(top-level-prove ',(replace-?-vars goals)))
  `(begin
	 (format #t "> ~a\n" (cons '?- ',goals))
	 (top-level-prove ',(replace-?-vars goals))))
