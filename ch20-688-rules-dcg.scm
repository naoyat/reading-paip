(rule (S (?pred ?subj)) -->
	  (NP ?agr ?subj)
	  (VP ?agr ?pred))

(rule (NP ?agr (?det ?n)) -->
	  (Det ?agr ?det)
	  (N ?agr ?n))

(rule (NP 3sg (the male))          --> (:word he))
(rule (NP ~3sg (some objects))     --> (:word they))
(rule (VP 3sg sleep)               --> (:word sleeps))
(rule (VP ~3sg sleep)              --> (:word sleep))
(rule (Det ?any the)               --> (:word the))
(rule (N 3sg (young male human))   --> (:word boy))
(rule (N 3sg (young female human)) --> (:word girl))

(<- (funcall (lambda (?x) ?body) ?x ?body))

(rule (S ?sem) -->
	  (NP ?agr ?subj)
	  (VP ?agr ?pred)
	  (:test (funcall ?pred ?subj ?sem)))

;; p.694
(rule (S ?pred) -->
	  (NP ?agr ?subj)
	  (VP ?agr ?subj ?pred))


(rule (VP ?agr ?subj ?pred) -->
	  (Verb/tr ?agr ?subj ?pred ?obj)
	  (NP ?any-agr ?obj))

(rule (VP ?agr ?subj ?pred) -->
	  (Verb/intr ?agr ?subj ?pred))

(rule (Verb/tr ~3sg ?x (kiss ?x ?y) ?y) --> (:word kiss))
(rule (Verb/tr  3sg ?x (kiss ?x ?y) ?y) --> (:word kisses))
(rule (Verb/tr ?any ?x (kiss ?x ?y) ?y) --> (:word kissed))

(rule (Verb/intr ~3sg ?x (sleep ?x)) --> (:word sleep))
(rule (Verb/intr  3sg ?x (sleep ?x)) --> (:word sleeps))
(rule (Verb/intr ?any ?x (sleep ?x)) --> (:word slept))

;; Here are the rules for noun phrases and nouns:
(rule (NP ?agr ?sem) -->
	  (Name ?agr ?sem))

(rule (NP ?agr (?det-sem ?noun-sem)) -->
	  (Det ?agr ?det-sem)
	  (Noun ?agr ?noun-sem))

(rule (Name 3sg Terry) --> (:word Terry))
(rule (Name 3sg Jean) --> (:word Jean))
;;p.695
(rule (Noun 3sg (young male human)) --> (:word boy))
(rule (Noun 3sg (young female human)) --> (:word girl))
(rule (Noun ~3sg (group (young male human))) --> (:word boys))
(rule (Noun ~3sg (group (young female human))) --> (:word girls))

(rule (Det ?any the) --> (:word the))
(rule (Det 3sg a) --> (:word a))
