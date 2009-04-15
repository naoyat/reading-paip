(require "./ch04-14")

(use-ops (make-block-ops '(a b)))

(print
 (GPS '((a on table) (b on table) (space on a) (space on b)
        (space on table))
      '((a on b) (b on table)))
 )

(print
 (GPS '((a on b) (b on table) (space on a) (space on table))
      '((b on a)))
 )

(use-ops (make-block-ops '(a b c)))

(print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
      '((b on a) (c on b)))
 )
;(debug :gps)
(print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
	  '((c on b) (b on a)))
 )
;;

;;;;;;;;;;;;;
(print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
	  '((c on b) (b on a)))
 )

(print
 (GPS '((c on a) (a on table) (b on table)
		(space on c) (space on b) (space on table))
	  '((c on table)))
 )

(print
 (GPS '((c on a) (a on table) (b on table)
		(space on c) (space on b) (space on table))
	  '((c on table) (a on b)))
 )


;;;;;;;;;;;;
(print
 (GPS '((c on a) (a on table) (b on table)
		(space on c) (space on b) (space on table))
	  '((c on table) (a on b)))
 )

(print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
	  '((b on a) (c on b)))
 )

(print
 (GPS '((a on b) (b on c) (c on table) (space on a) (space on table))
	  '((c on b) (b on a)))
 )

(define start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table)))
(print (GPS start '((a on b) (b on c))))
(print (GPS start '((b on c) (a on b))))
