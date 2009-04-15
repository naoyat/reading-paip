;(require "./ch11-2")
(require "./ch11-3")

(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (likes ?x ?x))

(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (length () 0))
(<- (length #f 0))
(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))


;;

(?- (member 2 (1 2 3)))
(?- (member 2 (1 2 3 2 1)))
;(?- (member ?x (1 2 3)))

;(?- (member 2 ?list))
;;
#|
(?- (length (a b c d) ?n))

(?- (length ?list (1+ (1+ 0))))
(?- (length ?list ?n))

(?- (length ?l (1+ (1+ 0))) (member a ?l))
(?- (member a ?l) (length ?l (1+ (1+ 0))))
|#

(newline)