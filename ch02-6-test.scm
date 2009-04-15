(require "./ch02-6")

;;Exercise 2.3
(define *日本語文法*
  '((文 -> (主語 述語 句点))
	(主語 -> (名詞 格助詞1) (代名詞 格助詞1))
	(目的語 -> (名詞 格助詞4) (代名詞 格助詞4))
	(述語 -> (目的語 副詞? 他動詞) (副詞? 形容詞))
	(副詞* -> () (副詞 副詞*))
	(副詞? -> () (副詞))
	(名詞 -> (固有名詞))
	(副詞 -> いつも 時々 ちょっとだけ)
	(他動詞 -> 見た 呼んだ 叩いた 笑った)
	(形容詞 -> 美しい おもしろい 寒い 黒い)
	(固有名詞 -> 純一郎 太郎)
	(代名詞 -> 私 僕 我々 彼 彼女 皆さん あなた ぼく きみ)
	(格助詞1 -> は が)
	(格助詞4 -> を)
	(句点 -> 。)
	))


(define (show category)
  (map display (remove null? (generate category)))
  (newline))

(set! *grammar* *日本語文法*)
;(print (generate '文))
(dotimes (i 10) (show '文))
(newline)

(define *いつどこで*
  '((文 -> (いつ どこで だれが なにを どうした 句点))
	(いつ -> (時間))
	(どこで -> (場所 で))
	(だれが -> (人物 が))
	(なにを -> (名詞 を))
	(どうした -> (動詞))

	(時間 -> 朝 昼休みに さっき 昨日 今日 おととい 先週)
	(人物 -> 私 僕 君 あなた わたし きみ ぼく みんな 山田)
	(場所 -> 町 学校 道端 トイレ コンビニ)
	(名詞 -> 紅茶 ノート かばん 傘)
	(動詞 -> 買った 落とした なくした 見つけた 拾った 見た もらった 壊した)
	(句点 -> 。)
	))

(set! *grammar* *いつどこで*)
;(print (generate '文))
(dotimes (i 10) (show '文))
(newline)

(define *yharian-grammar*
  '((sentence -> (phrase+ period))
	(phrase+ -> (phrase) (phrase comma phrase+))
	(phrase -> (word+))
	(word+ -> (word) (word space word+))
	(word -> y hara)
	(space -> " ")
	(comma -> ", ")
	(period -> "." ! ?)))

(set! *grammar* *yharian-grammar*)
(show 'sentence)
(newline)


(use gauche.test)
(test-start "combine-all, using cross-product")
(test* "((a) (b)) x ((1) (2))"
	   '((a 1) (b 1) (a 2) (b 2))
	   (combine-all '((a) (b)) '((1) (2)))
	   )
(test* "(1 2 3) x (10 20 30), using #'+"
	   '(11 12 13 21 22 23 31 32 33)
	   (cross-product + '(1 2 3) '(10 20 30)))
(test* "(a b c d e f g h) x (1 2 3 4 5 6 7 8), using #'list"
	   '((a 1) (b 1) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)
		 (a 2) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2) (h 2)
		 (a 3) (b 3) (c 3) (d 3) (e 3) (f 3) (g 3) (h 3)
		 (a 4) (b 4) (c 4) (d 4) (e 4) (f 4) (g 4) (h 4)
		 (a 5) (b 5) (c 5) (d 5) (e 5) (f 5) (g 5) (h 5)
		 (a 6) (b 6) (c 6) (d 6) (e 6) (f 6) (g 6) (h 6)
		 (a 7) (b 7) (c 7) (d 7) (e 7) (f 7) (g 7) (h 7)
		 (a 8) (b 8) (c 8) (d 8) (e 8) (f 8) (g 8) (h 8))
	   (cross-product list '(a b c d e f g h) '(1 2 3 4 5 6 7 8)))
(test-end)

