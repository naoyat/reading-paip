(require "./debug")

(use srfi-1) ;optional [=] for (member)
(use srfi-13) ;string-titlecase
(use math.const)

(define fail cl:nil)

(define (tree-search states goal? successors combiner)
  (dbg :search ";; Search: ~a" states)
  (cond [(null? states) fail]
        [(goal? (first states)) (first states)]
        [else (tree-search (combiner (successors (first states))
                                     (rest states))
                           goal?
                           successors
                           combiner)]))

(define (depth-first-search start goal? successors)
  (tree-search (list start) goal? successors append))

(define (binary-tree x) (list (* 2 x) (+ 1 (* 2 x))))

(define (is value) (cut eqv? <> value)) ;(lambda (x) (eqv? x value)))

;;;
(define (prepend x y) (append y x))

(define (breadth-first-search start goal? successors)
  (tree-search (list start) goal? successors prepend))

;;;
(define (finite-binary-tree n)
  (lambda (x) (remove (cut > <> n) (binary-tree x))))

;;;
(define (diff num)
  (lambda (x) (abs (- x num))))

(define (sorter cost-fn)
  (lambda (new old)
    (sort (append new old)
          (lambda (a b) (< (cost-fn a) (cost-fn b)))
          )))

(define (best-first-search start goal? successors cost-fn)
  (tree-search (list start) goal? successors (sorter cost-fn)))

(define cl:most-positive-fixnum (greatest-fixnum))
;;;
(define (price-is-right price)
  (lambda (x) (if (> x price)
                  cl:most-positive-fixnum
                  (- price x))))

;;;
(define (beam-search start goal? successors cost-fn beam-width)
  ;(dbg :beam "(beam-search ~a ... ~d)" start beam-width)
  (tree-search (list start) goal? successors
               (lambda (old new)
                 (let1 sorted ((sorter cost-fn) old new)
                   (if (> beam-width (length sorted))
                       sorted
                       (subseq sorted 0 beam-width))))))

;(debug :search)
;(depth-first-search 1 (is 12) binary-tree)
;(breadth-first-search 1 (is 12) binary-tree)
;(depth-first-search 1 (is 12) (finite-binary-tree 15))
;(best-first-search 1 (is 12) binary-tree (diff 12))
;(best-first-search 1 (is 12) binary-tree (price-is-right 12))
;(beam-search 1 (is 12) binary-tree (price-is-right 12) 2)
;(beam-search 1 (is 12) binary-tree (diff 12) 2)

;(define-class <city> () (name long lat)) ;;(type list)
(define (city name) (assoc name *cities*)) ;; Find the city with this name.
(define (city-name city) (first city))
(define (city-long city) (second city))
(define (city-lat city) (third city))

(define *cities*
  '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)
    (Chicago      87.37 41.50) (New-York       73.58 40.47)
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27)
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

;; p198
(define (neighbors city)
  (filter (lambda (c) (and (not (eq? c city))
                           (< (air-distance c city) 1000.0)))
          *cities*))

(define (trip start dest)
  ;; Search for a way from the start to dest.
  (beam-search start (is dest) neighbors
               (cut air-distance <> dest)
               1))

;; p201から先取り
(define earth-diameter 12765.0)
(define (air-distance city1 city2)
  (let1 d (distance (xyz-coords city1) (xyz-coords city2))
    (* earth-diameter (asin (/ d 2)))))
(define (xyz-coords city)
  (let ([psi (deg->radians (city-lat city))]
        [phi (deg->radians (city-long city))])
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))
(define (distance point1 point2)
  (sqrt (apply + (map (lambda (a b) (expt (- a b) 2)) point1 point2))))
(define (deg->radians deg)
;  (* (+ (truncate deg) (* (remainder deg 1) 100/60)) pi 1/180))
  (receive (frac int) (modf deg)
    (* (+ int (* frac 100/60)) pi/180)))
#|
#?=(trip (city 'San-Francisco) (city 'Boston))
#?=(trip (city 'Boston) (city 'San-Francisco))
|#
(define-class <path> () ;; print-path
  ((state       :init-keyword :state       :init-value #f)
   (previous    :init-keyword :previous    :init-value '())
   (cost-so-far :init-keyword :cost-so-far :init-value 0)
   (total-cost  :init-keyword :total-cost  :init-value 0)))

(define path-state (cut slot-ref <> 'state))
(define path-previous (cut slot-ref <> 'previous))
(define path-cost-so-far (cut slot-ref <> 'cost-so-far))
(define path-total-cost (cut slot-ref <> 'total-cost))

(define-method write-object ((path <path>) port)
  (print-path path port))

(define (trip start dest . args)
  (let-optionals* args ((beam-width 1))
    (beam-search
     (make <path> :state start)
     (is dest :key path-state)
     (path-saver neighbors air-distance (cut air-distance <> dest))
     path-total-cost
     beam-width)))

(define (is value . args)
  (let-keywords* args ((key identity)
                       (test eqv?))
      (lambda (path) (test value (key path)))))
(define (path-saver successors cost-fn cost-left-fn)
  (lambda (old-path)
    (let1 old-state (path-state old-path)
      (map (lambda (new-state)
             (let1 old-cost (+ (path-cost-so-far old-path)
                               (cost-fn old-state new-state))
               (make <path>
                 :state new-state
                 :previous old-path
                 :cost-so-far old-cost
                 :total-cost (+ old-cost (cost-left-fn new-state)) )))
           (successors old-state)
           ))))

(define (print-path path . args)
  (let-optionals* args ((stream #t))
    ;; (depth #f))
    ;; (declare (ignore depth))
    (format stream "#<Path to ~a cost ~d>"
            (path-state path) (path-total-cost path))))
(define (show-city-path path . args)
  (let-optionals* args ((stream #t))
    ;;(format stream "#<Path ~d km: ~{~:(~a~)~^ - ~}>" (path-total-cost path)
    (format stream "#<Path ~d km: ~a>" (path-total-cost path)
            (string-join (map (compose string-titlecase x->string) (reverse (map-path city-name path))) " - "))
    (values)))
(define (map-path fn path)
  (if (null? path) '()
      (cons (fn (path-state path))
            (map-path fn (path-previous path)))))
#|
(show-city-path (trip (city 'San-Francisco) (city 'Boston) 1) #t)
(newline)
(show-city-path (trip (city 'Boston) (city 'San-Francisco) 1) #t)
(newline)
(show-city-path (trip (city 'Boston) (city 'San-Francisco) 3) #t)
(newline)
|#

;; p204
(define (iter-wide-search start goal? successors cost-fn . args)
  (let-keywords* args ((width 1)
                       (max 100))
    (dbg :search "; Width: ~d" width)
    (unless (> width max)
      (or (cl:thru (beam-search start goal? successors cost-fn width))
          (iter-wide-search start goal? successors cost-fn :width (+ width 1) :max max)))))

;(debug :search)
;#?=(iter-wide-search 1 (is 12) (finite-binary-tree 15) (diff 12))

;; p206
(define (graph-search states goal? successors combiner . args)
  (let-optionals* args ((state= eqv?)
                        (old-states #f))
    (dbg :search ";; Search: ~a" states)
    (cond [(null? states) fail]
          [(goal? (first states)) (first states)]
          [else (graph-search (combiner (new-states states successors state= old-states)
                                        (rest states))
                              goal? successors combiner state=
                              (lset-adjoin state= (first states) old-states :test state=))])))
(define (new-states states successors state= old-states)
  (remove (lambda (state) (or (member state states state=)
                              (member state old-states state=)))
          (successors (first states))))

(define (next2 x) (list (+ x 1) (+ x 2)))
#|
#?=(tree-search '(1) (is 6) next2 prepend)
#?=(graph-search '(1) (is 6) next2 prepend)
|#

;; A*
(define (a*-search paths goal? successors cost-fn cost-left-fn . args)
  (let-optionals* args ((state= eqv?)
                        (old-paths '()))
    (dbg :search ";; Search: ~a" paths)
    (cond [(null? paths) fail]
          [(goal? (path-state (first paths)))
           (values (first paths) paths)]
          [else (let* ([path (pop! paths)]
                       [state (path-state path)])
                  (set! old-paths (insert-path path old-paths))
                  (dolist (state2 (successors state))
                    (let* ([cost (+ (path-cost-so-far path)
                                    (cost-fn state state2))]
                           [cost2 (cost-left-fn state2)]
                           [path2 (make <path>
                                    :state state2
                                    :previous path
                                    :cost-so-far cost
                                    :total-cost (+ cost cost2))]
                           [old #f])
                      (cond [(cl:thru (cl:setf old (find-path state2 paths state=)))
                             (when (better-path path2 old)
                               (set! paths (insert-path path2 (delete old paths))))]
                            [(cl:thru (cl:setf old (find-path state2 old-paths state=)))
                             (when (better-path path2 old)
                               (set! paths (insert-path path2 paths))
                               (set! old-paths (delete old old-paths)))]
                            [else
                             (set! paths (insert-path path2 paths))])))
                  (a*-search paths goal? successors cost-fn cost-left-fn state= old-paths))])))

(define (find-path state paths state=)
  #;(format #t "(find-path ~a ~a state=)..\n" state paths)
  (cl:find state paths :key path-state :test state=))

(define (better-path path1 path2)
;  (format #t "(better-path ~a ~a)...\n" path1 path2)
  (< (path-total-cost path1) (path-total-cost path2)))
(define (insert-path path paths)
  (cl:merge 'list (list path) paths < :key path-total-cost))
(define (path-states path)
  (if (null? path) '()
      (cons (path-state path)
            (path-states (path-previous path)))))

#;(path-states (a*-search (list (make <path> :state 1))
                           (is 6)
                           next2
                           (lambda (x y) 1)
                           (diff 6)))

(define (search-all start goal? successors cost-fn beam-width)
  (let1 solutions '()
    (beam-search start (lambda (x) (when (goal? x) (cl:push x solutions)) #f)
                 successors cost-fn beam-width)
    solutions))
