(require "./ch08-2")

(pat-match-abbrev 'n '(?is n number?))
(pat-match-abbrev 'm '(?is m number?))
(pat-match-abbrev 's '(?is s not-number?))

(define (not-number? x) (not (number? x)))

(define (simp-rule rule)
  (let1 exp (infix->prefix rule)
    (make-exp (expand-pat-match-abbrev (exp-lhs exp))
              (exp-op exp) (exp-rhs exp))))

(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '((s * n = n * s)
                     (n * (m * x) = (n * m) * x)
                     (x * (n * y) = n * (x * y))
                     ((n * x) * y = n * (x * y))
                     (n + s = s + n)
                     ((x + m) + n = x + n + m)
                     (x + (y + n) = (x + y) + n)
                     ((x + n) + y = (x + y) + n) ))))

