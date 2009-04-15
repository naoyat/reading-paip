(require "./ch19-4")

(use gauche.test)
(test-start "§ 19.4")

(test-section "parser")
(test* "John liked Mary"
	   '((S (NP (Name John))
			(VP (V liked) (NP (Name Mary)))))
	   (parser '(John liked Mary)) )

(test* "Dana liked Dale"
	   '((S (NP (Name Dana))
			(VP (V liked) (NP (Name Dale)))))
	   (parser '(Dana liked Dale)) )

(test* "the rab zaggled the woogly quax"
	   '((S (NP (D the) (N rab))
			(VP (V zaggled) (NP (D the) (A+ (A woogly)) (N quax)))))
	   (parser '(the rab zaggled the woogly quax)) )

;;
(test* "the slithy toves gymbled"
	   '((S (NP (D the) (N slithy))
			(VP (V toves) (NP (Name gymbled))))
		 (S (NP (D the) (A+ (A slithy)) (N toves))
			(VP (V gymbled)))
		 (NP (D the) (A+ (A slithy) (A+ (A toves))) (N gymbled)))
	   (parser '(the slithy toves gymbled)) )

(test-end)

