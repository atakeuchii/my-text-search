(ns my-text-search.highlight
  (:require [my-text-search.tokenizer :as tok]
            [clojure.string :as str]))

(defn- ranges-of
  "text 中の needle の全出現を [start end) で返す（昇順）。"
  [^String text ^String needle]
  (if (str/blank? needle)
    []
    (loop [from 0
           acc []]
      (let [i (.indexOf text needle from)]
        (if (neg? i)
          acc
          (recur (+ i (count needle)) (conj acc [i (+ i (count needle))])))))))

(defn find-ranges
  "正規化テキスト中の、各 needle(正規化済み)の出現範囲を昇順・非重複にまとめて返す。
   重複・隣接する範囲はマージする。"
  [^String norm-text needles]
  (let [all (sort (mapcat #(ranges-of norm-text %) needles))]
    (reduce (fn [acc [s e]]
              (if (and (seq acc) (<= s (second (peek acc))))
                (update acc (dec (count acc)) (fn [[ps pe]] [ps (max pe e)]))
                (conj acc [s e])))
            []
            all)))

(defn mark
  "text の ranges(昇順・非重複)を pre/post で囲んだ文字列を返す。"
  [^String text ranges pre post]
  (let [sb (StringBuilder.)]
    (loop [prev 0
           rs ranges]
      (if-let [[s e] (first rs)]
        (do (.append sb (subs text prev s))
            (.append sb pre)
            (.append sb (subs text s e))
            (.append sb post)
            (recur e (rest rs)))
        (do (.append sb (subs text prev))
            (.toString sb))))))

(defn snippet
  "text から query のマッチ周辺を抜き出し、マッチを囲んだスニペットを返す。
   範囲特定は正規化テキスト上で行うため、スニペットは正規化テキストから作る。"
  [text query & {:keys [window pre post ellipsis]
                 :or {window 20 pre "《" post "》" ellipsis "…"}}]
  (let [nt (tok/normalize text)
        ranges (find-ranges nt [(tok/normalize query)])]
    (if (empty? ranges)
      (let [end (min (count nt) window)]
        (str (subs nt 0 end) (when (< end (count nt)) ellipsis)))
      (let [[fs fe] (first ranges)
            start (max 0 (- fs (quot window 2)))
            end (min (count nt) (+ fe (quot window 2)))
            in-window (->> ranges
                           (filter (fn [[s e]] (and (>= s start) (<= e end))))
                           (map (fn [[s e]] [(- s start) (- e start)])))
            slice (subs nt start end)]
        (str (when (> start 0) ellipsis)
             (mark slice in-window pre post)
             (when (< end (count nt)) ellipsis))))))

(defn with-snippets
  "ランキング結果 rows の各行に :snippet を付ける。
   doc-text-fn: doc-id -> 本文。opts は snippet に渡す(:window :pre :post 等)。"
  [doc-text-fn query rows & opts]
  (map (fn [row]
         (assoc row :snippet
                (apply snippet (or (doc-text-fn (:doc-id row)) "") query opts)))
       rows))
