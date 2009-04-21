;;
;; 20.2
;;
(rule (S) --> (NP) (VP))

(rule (S ?sem) --> (NP ?subj) (VP ?pred)
	  (:test (combine ?subj ?pred ?sem)))

;; verb --> [sleeps].

(rule (NP (the male) 3sg) --> (:word he))
(rule (VP sleeps 3sg) --> (:word sleeps))
