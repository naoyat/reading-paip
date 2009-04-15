(require "./ch06-5")

(use gauche.test)
(test-start "§6.5")

;;
(define start '((c on a) (a on table) (b on table) (space on c) (space on b) (space on table)))

(use-ops (make-block-ops '(a b c)))

;; %/C/A// %/B// %//
(test-section "gps-successors")
(test* ""
	   '(((start)
		  (a on table) (b on table) (space on c) (space on b) (space on table)
		  ;; A// %B// %C //
		  (executing (move c from a to table)) ; C/A -> C//
		  (c on table) (space on a)) ; C// %/A
		 ((start)
		  (a on table) (b on table) (space on c) (space on table)
		  (executing (move c from a to b))
		  (c on b) (space on a))
		 ((start)
		  (c on a) (a on table) (space on b) (space on table)
		  (executing (move b from table to c))
		  (b on c)) )
	   (gps-successors `((start) ,@start)))
#|
(test-section "applicable-ops")
(test* "" '() (applicable-ops `((start) ,@start)))
|#

(test-section "search-gps")
(test* ""
	   '((start)
		 (executing (move c from a to table))
		 (executing (move b from table to c))
		 (executing (move a from table to b)))
	   (search-gps start '((a on b) (b on c))) )
(test* ""
	   '((start)
		 (executing (move c from a to table))
		 (executing (move b from table to c))
		 (executing (move a from table to b)))
	   (search-gps start '((b on c) (a on b))) )

(test-end)
