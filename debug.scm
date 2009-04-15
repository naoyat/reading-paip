(require "./cl-emu")
(use srfi-13) ;string-upcase

(define *dbg-ids* '())
(define *debug-io* (current-output-port))

(define (dbg id format-string . args)
  (when (member id *dbg-ids*)
    (apply format *debug-io* format-string args)
    (newline *debug-io*)
	))
(define (debug . ids)
  (set! *dbg-ids* (lset-union eq? ids *dbg-ids*)))
(define (undebug . ids)
  (set! *dbg-ids* (if (null? ids) '()
                      (lset-difference eq? *dbg-ids* ids))))
(define (dbg-indent id indent format-string . args)
  (when (member id *dbg-ids*)
    (dotimes (i indent) (display "  " *debug-io*))
    (apply format *debug-io* format-string args)
    (newline *debug-io*) ))

(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define (trace-in fn . args)
  (dbg :trace "(~a ~a)" (symbol-upcase fn) args))
(define (trace-out fn rv)
  (dbg :trace "~a returned ~a" (symbol-upcase fn) rv))
