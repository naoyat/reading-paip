(define (rot13 str)
  (list->string (map (lambda (ch)
					   (let1 co (char->integer ch)
						 (integer->char
						  (cond [(<= 65 co 77) (+ co 13)]
								[(<= 78 co 90) (- co 13)]
								[(<= 97 co 109) (+ co 13)]
								[(<= 110 co o122) (- co 13)]
								[else co]))))
					 (string->list str))))

(print (rot13 "Pnssrvar vf whfg n fyrrcvat qeht sbe zr gbb. Abj cynlvat"))
