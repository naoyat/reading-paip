(<- (S (?pred ?subj) ?s0 ?s2)
	(NP ?agr ?subj ?s0 ?s1)
	(VP ?agr ?pred ?s1 ?s2))

(<- (NP 3sg (the male) (he . ?s) ?s))
(<- (NP ~3sg (some objects) (they . ?s) ?s))

(<- (NP ?agr (?det ?n) ?s0 ?s2)
	(Det ?agr ?det ?s0 ?s1)
	(N ?agr ?n ?s1 ?s2))
(<- (VP 3sg sleep (sleeps . ?s) ?s))
(<- (VP ~3sg sleep (sleep . ?s) ?s))

(<- (Det ?any the (the . ?s) ?s))
(<- (N 3sg (young male human) (boy . ?s) ?s))
(<- (N 3sg (young female human) (girl . ?s) ?s))

(<- (S (?pred ?subj) (s ?np ?vp) ?s0 ?s2)
	(NP ?agr ?subj ?np ?s0 ?s1)
	(VP ?agr ?pred ?vp ?s1 ?s2))
(<- (NP 3sg (the male) (np he) (he . ?s) ?s))
(<- (NP ~3sg (some objects) (np they) (they . ?s) ?s))
;; p.689
(<- (NP ?agr (?det ?n) (np ?det-syn ?n-syn) ?s0 ?s2)
	(Det ?agr ?det ?det-syn ?s0 ?s1)
	(N ?agr ?n ?n-syn ?s1 ?s2))
(<- (VP 3sg sleep (vp sleeps) (sleeps . ?s) ?s))
(<- (VP ~3sg sleep (vp sleep) (sleep . ?s) ?s))

(<- (Det ?any the (det the) (the . ?s) ?s))
(<- (N 3sg (young male human) (n boy) (boy . ?s) ?s))
(<- (N 3sg (young female human) (n girl) (girl . ?s) ?s))
