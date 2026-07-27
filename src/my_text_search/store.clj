(ns my-text-search.store
  (:require [my-storage.core :as lsm]
            [my-text-search.codec :as codec])
  (:import [java.util Base64]))

(def ^:private next-id-key "m:next-id")
(def ^:private total-len-key "m:total-len")
(defn- term-key ^String [term] (str "t:" term))
(defn- word-key ^String [word] (str "w:" word))
(defn- doc-key  ^String [doc-id] (str "d:" doc-id))
(defn- len-key  ^String [doc-id] (str "l:" doc-id))

(defn- bytes->str
  ^String [^bytes bs]
  (.encodeToString (Base64/getEncoder) bs))

(defn- str->bytes
  ^String [^String s]
  (.decode (Base64/getDecoder) s))

(defn put-posting!
  [store term doc->tf]
  (lsm/put store (term-key term) (bytes->str (codec/encode-posting doc->tf))))

(defn put-word!
  [store word doc->tf]
  (lsm/put store (word-key word) (bytes->str (codec/encode-posting doc->tf))))

(defn get-word
  [store word]
  (when-let [s (lsm/get store (word-key word))]
    (codec/decode-posting (str->bytes s))))

(defn scan-words
  [store prefix]
  (let [start (word-key prefix)]
    (->> (lsm/scan store start nil)
         (take-while (fn [[k _]] (.startsWith ^String k start)))
         (map (fn [[k v]]
                [(subs k (count "w:"))
                 (codec/decode-posting (str->bytes v))])))))

(defn get-posting
  [store term]
  (when-let [s (lsm/get store (term-key term))]
    (codec/decode-posting (str->bytes s))))

(defn put-doc! 
  [store doc-id text]
  (lsm/put store (doc-key doc-id) text))

(defn get-doc
  [store doc-id]
  (lsm/get store (doc-key doc-id)))

(defn get-next-id
  [store]
  (if-let [s (lsm/get store next-id-key)]
    (Long/parseLong s)
    0))

(defn set-next-id!
  [store n]
  (lsm/put store next-id-key (str n)))

(defn put-doc-length!
  [store doc-id len]
  (lsm/put store (len-key doc-id) (str len)))

(defn get-doc-length
  [store doc-id]
  (if-let [s (lsm/get store (len-key doc-id))]
    (Long/parseLong s)
    0))

(defn set-total-len!
  [store n]
  (lsm/put store total-len-key (str n)))

(defn get-total-len
  [store]
  (if-let [s (lsm/get store total-len-key)]
    (Long/parseLong s)
    0))

(defn doc-count
  [store]
  (get-next-id store))

(defn persist-index!
  [store index]
  (doseq [[term ids] (:postings index)]
    (put-posting! store term ids))
  (doseq [[word ids] (:words index)]
    (put-word! store word ids))
  (doseq [[id text] (:docs index)]
    (put-doc! store id text))
  (doseq [[id len] (:doc-lengths index)]
    (put-doc-length! store id len))
  (set-total-len! store (reduce + 0 (vals (:doc-lengths index))))
  (set-next-id! store (:next-id index))
  store)

(defn all-words
  [store]
  (->> (lsm/scan store "w:" "x")
       (map (fn [[k _]] (subs k (count "w:"))))))

(defn avg-doc-length
  [store]
  (let [n (doc-count store)]
    (if (zero? n)
      0.0
      (/ (double (get-total-len store)) n))))
