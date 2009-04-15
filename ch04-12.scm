(require "./ch04-11")
(use gauche.interactive)

(define *banana-ops*
  (list
   (make <op>
	 :action 'climb-on-chair
	 :preconds '(chair-at-middle-room at-middle-room on-floor)
	 :add-list '(at-bananas on-chair)
	 :del-list '(at-middle-room on-floor))
   (make <op>
	 :action 'push-chair-from-door-to-middle-room
	 :preconds '(chair-at-door at-door)
	 :add-list '(chair-at-middle-room at-middle-room)
	 :del-list '(chair-at-door at-door))
   (make <op>
	 :action 'walk-from-door-to-middle-room
	 :preconds '(at-door on-floor)
	 :add-list '(at-middle-room)
	 :del-list '(at-door))
   (make <op>
	 :action 'grasp-bananas
	 :preconds '(at-bananas empty-handed)
	 :add-list '(has-bananas)
	 :del-list '(empty-handed))
   (make <op>
	 :action 'drop-ball
	 :preconds '(has-ball)
	 :add-list '(empty-handed)
	 :del-list '(has-ball))
   (make <op>
	 :action 'eat-bananas
	 :preconds '(has-bananas)
	 :add-list '(empty-handed not-hungry)
	 :del-list '(has-bananas hungry))
   ))

(use-ops *banana-ops*)
#;(GPS '(at-door on-floor has-ball hungry chair-at-door)
	 '(not-hungry))
