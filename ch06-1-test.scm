(require "./ch05-4") ; flatten
(require "./ch06-1")
(interactive-interpreter (prompt-generator) (compose flatten use-eliza-rules))
