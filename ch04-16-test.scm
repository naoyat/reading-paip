(require "./ch04-14")

(use-ops (cl:push (make <op>
                    :action 'taxi-son-to-school
                    :preconds '(son-at-home have-money)
                    :add-list '(son-at-school)
                    :del-list '(son-at-home have-money))
                  *school-ops*))
(debug :gps)

(map print
     (GPS '(son-at-home have-money car-works)
          '(son-at-school have-money))
     )

;;;4.17
#;(make <op>
  :action '(push X from A to B)
  :preconds '((monkey at A) (X at A) (pushable X) (path A B))
  :add-list '((monkey at B) (X at B))
  :del-list '((monkey at A) (X at A)))

