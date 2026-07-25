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

(defn segments
  "正規化したテキストを、検索対象文字の連なり(セグメント=語のかたまり)に分割する。
   空白・記号は境界として除かれる。ワイルドカード/ファジー用の語辞書に使う。"
  [^String s]
  (re-seq token-char-pattern (normalize s)))

(defn tokenize
  ([s] (tokenize 2 s))
  ([n s]
   (mapcat #(segment->ngrams n %) (segments s))))
