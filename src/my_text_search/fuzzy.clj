(ns my-text-search.fuzzy
  (:require [my-text-search.query :as q]))

(defn levenshtein
  "a と b のレーベンシュタイン距離（挿入・削除・置換のコスト1）。
   2行DPで計算し、メモリは O(min(m,n))。"
  ^long [^String a ^String b]
  (let [m (.length a)
        n (.length b)]
    (cond
      (zero? m) n
      (zero? n) m
      :else (loop [i 1
                   prev (vec (range (inc n)))]
              (if (> i m)
                (peek prev)
                (let [ca (.charAt a (dec i))
                      cur (loop [j 1
                                 row (transient [i])]
                            (if (> j n)
                              (persistent! row)
                              (let [cb (.charAt b (dec j))
                                    cost (if (= ca cb) 0 1)
                                    v (min (inc (nth prev j))
                                           (inc (nth row (dec j)))
                                           (+ (nth prev (dec j)) cost))]
                                (recur (inc j) (conj! row v)))))]
                  (recur (inc i) cur)))))))

(defn within?
  "a と b の編集距離が k 以下かを判定する。
   長さ差による枝刈りと、行の最小値による早期打ち切りを行う。"
  [^String a ^String b ^long k]
  (let [m (.length a)
        n (.length b)]
    (if (> (Math/abs (- m n)) k)
      false
      (loop [i 1
             prev (vec (range (inc n)))]
        (if (> i m)
          (<= (peek prev) k)
          (let [ca (.charAt a (dec i))
                cur (loop [j 1
                           row (transient [i])]
                      (if (> j n)
                        (persistent! row)
                        (let [cb (.charAt b (dec j))
                              cost (if (= ca cb) 0 1)
                              v (min (inc (nth prev j))
                                     (inc (nth row (dec j)))
                                     (+ (nth prev (dec j)) cost))]
                          (recur (inc j) (conj! row v)))))]
            (if (> (apply min cur) k)
              false
              (recur (inc i) cur))))))))

(defn fuzzy-terms-naive
  "全語を走査し、query と距離 k 以下の語を集める（枝刈りなしの素朴版・速度比較用）。
   words: 語の seq。"
  [words query k]
  (filter #(<= (levenshtein query %) k) words))

(defn fuzzy-terms
  [words query k]
  (filter #(within? query % k) words))

(defn fuzzy-search
  [words word-posting-fn query k]
  (->> (fuzzy-terms words query k)
       (map word-posting-fn)
       q/or-postings))
