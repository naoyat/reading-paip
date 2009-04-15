(require "./debug")
;(require "./ch05-2")
#|
(define (eliza)
  (loop
   (print 'eliza>)
   (print (flatten (use-eliza-rules (read))))))
|#
(define-macro (loop . body)
  (let1 loopname (gensym)
    `(let ,loopname () ,@body (,loopname))))

(define-macro (handler-case expression . typespecs)
  (let* ([ts0 (car typespecs)]
         [ts0-val (caadr ts0)]
         [ts0-body (cddr ts0)])
    `(guard (,ts0-val (else ,@ts0-body))
       ,expression)))

(define (interactive-interpreter prompt transformer)
  (loop ;;(let loop ()
    (handler-case
     (begin
       (if (string? prompt)
           (display prompt)
           (prompt))
       (display (transformer (read)))
       ;(loop))
       )
     (error (condition)
            (format #t "\n;; Error ~a ignored, back to top level.\n" condition)
            ))))

(define (prompt-generator . args)
  (let-optionals* args ((num 0)
                        (ctl-string "[~d] "))
      (lambda () (format #t ctl-string (inc! num)) (flush))))

;(require "./ch05-4")
;(interactive-interpreter (prompt-generator) (compose flatten use-eliza-rules))

