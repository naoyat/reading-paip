(require "./cl-emu")

(define (文) (append (名詞句) (動詞句)))
(define (名詞) (one-of '(男 球 女 机)))
(define (動詞) (one-of '(打った 取った 見た 好んだ)))
(define (助詞) (one-of '(へ で と に)))
(define (形容詞) (one-of '(大きい 小さい 青い 緑の 断熱の)))
(define (名詞句) (append (形容詞*) (名詞) (助詞)))
(define (動詞句) (append (動詞) (名詞句)))
(define (形容詞*)
  (if (= 0 (cl:random 2))
	  cl:nil
	  (append (形容詞) (形容詞*))))

(define (one-of set)
  (list (random-elt set)))
(define (random-elt choices)
  (cl:elt choices (cl:random (length choices))))

(print (文))
