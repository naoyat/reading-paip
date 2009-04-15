(require "./cl-emu")
;;

(define (文) (append (名詞句) (動詞句)))
(define (名詞句) (append (冠詞) (名詞)))
(define (動詞句) (append (動詞) (名詞句)))
(define (冠詞) (１つ選ぶ '(the a)))
(define (名詞) (１つ選ぶ '(man ball woman table)))
(define (動詞) (１つ選ぶ '(hit took saw liked)))

(define (１つ選ぶ 候補)
  "Pick one element of set, and make a list of it."
  (list (ランダム要素 候補)))

(define (ランダム要素 選択肢)
  "Choose an element from a list at random."
  (cl:elt 選択肢 (cl:random (length 選択肢))))

(dotimes (i 10)
  (print (文)))
