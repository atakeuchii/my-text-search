(ns my-text-search.tokenizer
  (:import [java.text Normalizer Normalizer$Form]
           [java.util Locale]))

(def ^:private token-char-pattern
  ;; 文字・数字・結合文字の連なりを1セグメントとし、空白/記号はここで境界になる
  #"[\p{L}\p{N}\p{M}]+")

(defn normalize
  ^String [^String s]
  (-> (Normalizer/normalize s Normalizer$Form/NFKC)
      (.toLowerCase Locale/ROOT)))

(defn- segment->ngrams
  [^long n ^String seg]
  (let [len (.length seg)]
    (if (<= len n)
      [seg]
      (mapv #(subs seg % (+ % n))
            (range (inc (- len n)))))))

(defn tokenize
  ([s] (tokenize 2 s))
  ([n s]
   (->> (normalize s)
        (re-seq token-char-pattern)
        (mapcat #(segment->ngrams n %)))))
