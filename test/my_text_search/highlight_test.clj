(ns my-text-search.highlight-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [my-text-search.highlight :as hl]
            [my-text-search.tokenizer :as tok]))

(deftest find-ranges-locates-matches
  (let [nt (tok/normalize "純米大吟醸の日本酒は美味しい日本酒だ")]
    (is (= [[6 9] [14 17]] (hl/find-ranges nt [(tok/normalize "日本酒")])))
    ;; 各範囲が実際に日本酒を指す
    (is (every? #(= "日本酒" (apply subs nt %))
                (hl/find-ranges nt [(tok/normalize "日本酒")])))))

(deftest find-ranges-merges-overlaps
  ;; bi-gram 断片(日本, 本酒)が重なって元の語の範囲にマージされる
  (let [nt (tok/normalize "日本酒")]
    (is (= [[0 3]] (hl/find-ranges nt [(tok/normalize "日本") (tok/normalize "本酒")])))))

(deftest find-ranges-normalization
  ;; 全角大文字クエリが正規化後の本文に当たる
  (let [nt (tok/normalize "ＳＡＫＥと日本酒")]
    (is (= [[0 4]] (hl/find-ranges nt [(tok/normalize "SAKE")])))))

(deftest find-ranges-no-match
  (is (= [] (hl/find-ranges (tok/normalize "日本酒") [(tok/normalize "焼酎")]))))

(deftest mark-wraps-ranges
  (is (= "《日本酒》は美味しい" (hl/mark "日本酒は美味しい" [[0 3]] "《" "》")))
  (is (= "[日本酒]と[焼酎]"    (hl/mark "日本酒と焼酎" [[0 3] [4 6]] "[" "]"))))

(deftest snippet-marks-and-windows
  (let [text "純米大吟醸として名高いこの酒は米を磨き抜いて造られた日本酒でありとても華やかな香りが特徴の日本酒です"
        s (hl/snippet text "日本酒" :pre "<m>" :post "</m>")]
    (is (str/includes? s "<m>日本酒</m>"))
    (is (str/includes? s "…"))
    (is (< (count s) (count text)))))

(deftest snippet-no-match-returns-head
  (is (= "純米大吟醸の焼酎です" (hl/snippet "純米大吟醸の焼酎です" "日本酒" :window 20))))

(deftest snippet-short-text-no-ellipsis
  (is (= "《日本酒》だ" (hl/snippet "日本酒だ" "日本酒"))))

(deftest with-snippets-attaches
  (let [dt {0 "華やかな日本酒です" 1 "すっきりした日本酒だ"}
        rows [{:doc-id 0 :score 1.0} {:doc-id 1 :score 0.5}]
        out (hl/with-snippets #(get dt %) "日本酒" rows :pre "<m>" :post "</m>")]
    (is (= [0 1] (map :doc-id out)))
    (is (every? #(str/includes? (:snippet %) "<m>日本酒</m>") out))))
