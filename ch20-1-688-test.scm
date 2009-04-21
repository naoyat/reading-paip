(require "./ch20-1")
;;
;; p.688
;;
(require "./ch20-688-rules")

;; Parsing:
(?- (S ?sem ?syn (he sleeps) ()))

;; Generating:
(?- (S (sleep (the male)) ? ?words ()))

;; Enumerating:
(?- (S ?sem ?syn ?words ()))
