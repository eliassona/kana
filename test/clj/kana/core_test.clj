(ns kana.core-test
  (:use clojure.test
        kana.crossover.core))

(deftest kana-to-hiragana
  (is (= "あ" (hiragana-of "a")))
  (is (= "あか" (hiragana-of "aka")))
  (is (= "しゃか" (hiragana-of "shaka")))
  )
(deftest kana-to-katakana
  (is (= "ア" (katakana-of "a")))
  (is (= "アカ" (katakana-of "aka")))
  (is (= "シャカ" (katakana-of "shaka")))
  )

(deftest hiragana-to-katakana
  (is (= "ア" (katakana-of "あ")))
  (is (= "アカ" (katakana-of "あか")))
  (is (= "シャカ" (katakana-of "しゃか")))
  )

(def rt (comp alphabet-of katakana-of hiragana-of))

(deftest roundtrip
  (is (= "a" (rt "a")))
  (is (= "aka" (rt "aka")))
  (is (= "shaka" (rt "shaka")))
  )
