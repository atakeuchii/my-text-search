(ns my-text-search.store
  (:require [my-storage.core :as lsm]
            [my-text-search.codec :as codec])
  (:import [java.util Base64]))

(def ^:private next-id-key "m:next-id")
(defn- term-key ^String [term] (str "t:" term))
(defn- word-key ^String [word] (str "w:" word))
(defn- doc-key  ^String [doc-id] (str "d:" doc-id))

(defn- bytes->str
  ^String [^bytes bs]
  (.encodeToString (Base64/getEncoder) bs))

(defn- str->bytes
  ^String [^String s]
  (.decode (Base64/getDecoder) s))

(defn put-posting!
  [store term doc-ids]
  (lsm/put store (term-key term) (bytes->str (codec/encode-posting doc-ids))))

(defn put-word!
  [store word doc-ids]
  (lsm/put store (word-key word) (bytes->str (codec/encode-posting doc-ids))))

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

(defn persist-index!
  [store index]
  (doseq [[term ids] (:postings index)]
    (put-posting! store term ids))
  (doseq [[word ids] (:words index)]
    (put-word! store word ids))
  (doseq [[id text] (:docs index)]
    (put-doc! store id text))
  (set-next-id! store (:next-id index))
  store)

(defn all-words
  [store]
  (->> (lsm/scan store "w:" "x")
       (map (fn [[k _]] (subs k (count "w:"))))))
