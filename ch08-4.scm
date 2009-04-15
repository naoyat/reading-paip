(require "./ch08-3")

(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '( (log 1         = 0)
                      (log 0         = undefined)
                      (log e         = 1)
                      (sin 0         = 0)
                      (sin pi        = 0)
                      (cos 0         = 1)
                      (cos pi        = -1)
                      (sin(pi / 2)   = 1)
                      (cos(pi / 2)   = 0)
                      (log (e ^ x)   = x)
                      (e ^ (log x)   = x)
                      ((x ^ y) * (x ^ z) = (x ^ (y + z)))
                      ((x ^ y) / (x ^ z) = (x ^ (y - z)))
                      (log x + log y = log(x * y))
                      (log x - log y = log(x / y))
                      ((sin x) ^ 2 + (cos x) ^ 2 = 1)
                      ))))

(define *infix->prefix-rules*
  (map expand-pat-match-abbrev
       '(((x+ = y+) (= x y))
         ((- x+)    (- x))
         ((+ x+)    (+ x))
         ((x+ + y+) (+ x y))
         ((x+ - y+) (- x y))
         ((d y+ / d x) (d y x)) ;;
         ((Int y+ d x) (int y x))
         ((x+ * y+) (* x y))
         ((x+ / y+) (/ x y))
         ((x+ ^ y+) (^ x y)))))

(set! *simplification-rules*
      (append *simplification-rules*
              (map simp-rule
                   '( (d x / d x       = 1)
                      (d (u + v) / d x = (d u / d x) + (d v / d x))
                      (d (u - v) / d x = (d u / d x) - (d v / d x))
                      (d (- u) / d x   = - (d u / d x))
                      (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
                      (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x))
                         / v ^ 2)
                      (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
                      (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                         + u ^ v * (log u) * (d v / d x))
                      (d (log u) / d x = (d u / d x) / u)
                      (d (sin u) / d x = (cos u) * (d u / d x))
                      (d (cos u) / d x = - (sin u) * (d u / d x))
                      (d (e ^ u) / d x = (e ^ u) * (d u / d x))
                      (d u / d x       = 0)))))
