(require "./prolog")

(define-macro (?- . goals)
;  `(top-level-prove ',(replace-?-vars goals)))
  `(begin
	 (format #t "> ~a\n" (cons '?- ',goals))
	 (top-level-prove ',(replace-?-vars goals))))

(<- (concat () ?l ?l))
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))

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
;;
(?- (S (he sleep) ()))

(<- (NP ?agr ?s0 ?s2)
	(Det ?agr ?s0 ?s1)
	(N ?agr ?s1 ?s2))
(<- (Det ?any (the . ?s) ?s))
(<- (N 3sg (boy . ?s) ?s))
(<- (N 3sg (girl . ?s) ?s))

;;w
(?- (S ?words ()))

;;; p.688
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

;; Parsing:
(?- (S ?sem ?syn (he sleeps) ()))

;; Generating:
(?- (S (sleep (the male)) ? ?words ()))

;; Enumerating:
(?- (S ?sem ?syn ?words ()))
