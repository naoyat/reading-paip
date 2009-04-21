(require "./ch20-1")

(<- (concat () ?l ?l))
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))

;(require "./ch20-2")

#;(<- (S ?s)
	(NP ?np)
	(VP ?vp)
	(concat ?np ?vp ?s))

(<- (S ?s)
	(concat ?np ?vp ?s)
	(NP ?np)
	(VP ?vp))

(<- (S ?s0 ?s2)
	(NP ?s0 ?s1)
	(VP ?s1 ?s2))

;;
(?- (S (the boy ate the apple) ()))

(<- (S ?s-in ?s-rem)
	(NP ?np-in ?np-rem)
	(VP ?vp-in ?vp-rem)
	(concat ?np-in ?np-rem ?vp-in ?vp-rem ?s-in ?s-rem))

(<- (concat ?a ?b  ?b ?c  ?a ?c))

(<- (S ?s0 ?s2)
	(NP ?agr ?s0 ?s1)
	(VP ?agr ?s1 ?s2))

(<- (NP 3sg (he . ?s) ?s))
(<- (NP ~3sg (they . ?s) ?s))

(<- (VP 3sg (sleeps . ?s) ?s))
(<- (VP ~3sg (sleep . ?s) ?s))

;;
(?- (S (he sleeps) ()))
(?- (S (he sleep) ()))

(<- (NP ?agr ?s0 ?s2)
	(Det ?agr ?s0 ?s1)
	(N ?agr ?s1 ?s2))
(<- (Det ?any (the . ?s) ?s))
(<- (N 3sg (boy . ?s) ?s))
(<- (N 3sg (girl . ?s) ?s))

;;w
(?- (S ?words ()))
