(require "./ch06-4")

(use gauche.test)
(test-start "§6.4")

;(debug :search)
(test-section "breadth-first search")
;(depth-first-search 1 (is 12) binary-tree)
(test* "" 12 (breadth-first-search 1 (is 12) binary-tree))

(test-section "depth-first search")
(test* "" 12 (depth-first-search 1 (is 12) (finite-binary-tree 15)))

(test-section "best-first search")
(test* "" 12 (best-first-search 1 (is 12) binary-tree (diff 12)))
(test* "" 12 (best-first-search 1 (is 12) binary-tree (price-is-right 12)))

(test-section "beam search")
(test* "" 12 (beam-search 1 (is 12) binary-tree (price-is-right 12) 2))
;(beam-search 1 (is 12) binary-tree (diff 12) 2)

(test-section "trip")
(test* "San Francisco -> Boston" "#<Path to (Boston 71.05 42.21) cost 4514.837101914012>"
	   (print-path (trip (city 'San-Francisco) (city 'Boston)) #f))
(test* "Boston -> San Francisco" "#<Path to (San-Francisco 122.26 37.47) cost 4577.299223830531>"
	   (print-path (trip (city 'Boston) (city 'San-Francisco)) #f))

(test-section "show-city-path")
(test* "San Francisco -> Boston, 1"
	   "#<Path 4514.837101914012 km: San-Francisco - Reno - Grand-Jct - Denver - Kansas-City - Indianapolis - Pittsburgh - Boston>"
	   (show-city-path (trip (city 'San-Francisco) (city 'Boston) 1) #f))
(test* "Boston -> San Francisco, 1"
	   "#<Path 4577.299223830531 km: Boston - Pittsburgh - Chicago - Kansas-City - Denver - Grand-Jct - Reno - San-Francisco>"
	   (show-city-path (trip (city 'Boston) (city 'San-Francisco) 1) #f))
(test* "Boston -> San Francisco, 3"
	   "#<Path 4514.837101914012 km: Boston - Pittsburgh - Indianapolis - Kansas-City - Denver - Grand-Jct - Reno - San-Francisco>"
	   (show-city-path (trip (city 'Boston) (city 'San-Francisco) 3) #f))

(debug :search)
(test-section "iter-wide-search")
(test* "is=12, finite binary tree 15, diff=12" 12
	   (iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12)))

(test-section "Searching Graphs: tree search vs. graph search")
(test* "tree search" 6 (tree-search '(1) (is 6) next2 prepend))
(test* "graph search" 6 (graph-search '(1) (is 6) next2 prepend))

(test-section "A* search")
(test* "" '(6 5 3 1)
	   (path-states (a*-search (list (make <path> :state 1))
							   (is 6)
							   next2
							   (lambda (x y) 1)
							   (diff 6))) )

(test-section "search-all")
#;(test* "" #f
	   (search-all 1;(list (make <path> :state 1))
				   (is 12)
				   next2
				   (price-is-right 12);(lambda (x y) 1)
				   1))
;(test* "" 12 (beam-search 1 (is 12) binary-tree (price-is-right 12) 2))

(test-end)

