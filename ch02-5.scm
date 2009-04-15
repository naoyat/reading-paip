(require "./ch02-3")

(define *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
	(noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
	(verb-phrase -> (Verb noun-phrase PP*))
	(PP* -> () (PP PP*))
	(Adj* -> () (Adj Adj*))
	(PP -> (Prep noun-phrase))
	(Prep -> (to in by with on))
	(Adj -> (big little blue green adiabatic))
	(Article -> the a)
	(Name -> Pat Kim Lee Terry Robin)
	(Noun -> man ball woman table)
	(Verb -> hit took saw liked)
	(Pronoun -> he she it these those that)
	))

#;(print (generate 'sentence))

(set! *grammar* *bigger-grammar*)
#;(print (generate 'sentence))
