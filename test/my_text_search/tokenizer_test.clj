(ns my-text-search.tokenizer-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.tokenizer :as tok]))

(deftest normalize-nfkc
  (is (= "abc123" (tok/normalize "ＡＢＣ１２３")))
  (is (= "アイウ" (tok/normalize "ｱｲｳ")))
  (is (= "ガ" (tok/normalize "ｶﾞ"))))

(deftest tokenize-bigram-basic
  (is (= ["日本" "本酒"] (tok/tokenize "日本酒"))))

(deftest tokenize-splits-on-separator
  (is (= ["赤い" "花"] (tok/tokenize "赤い 花")))
  (is (= ["a" "b"]     (tok/tokenize "A&B"))))

(deftest tokenize-short-segment-kept
  (is (= ["酒"] (tok/tokenize "酒"))))

(deftest tokenize-switch-n
  (is (= ["日" "本" "酒"] (tok/tokenize 1 "日本酒")))
  (is (= ["日本酒"]       (tok/tokenize 3 "日本酒"))))
