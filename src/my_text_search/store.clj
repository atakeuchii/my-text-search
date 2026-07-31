(ns my-text-search.store
  (:require [my-storage.core :as lsm]
            [my-text-search.codec :as codec]
            [my-text-search.index :as idx])
  (:import [java.util Base64]))

(def ^:private next-id-key "m:next-id")

(defn- fterm-key ^String [field term]   (str "t:" (name field) ":" term))
(defn- flen-key  ^String [field doc-id] (str "l:" (name field) ":" doc-id))
(defn- ftotal-key ^String [field]       (str "m:total-len:" (name field)))
(defn- fdoc-key  ^String [field doc-id] (str "d:" doc-id ":" (name field)))
(defn- word-key  ^String [word]         (str "w:" word))
(defn- vkey ^String [doc-id attr]       (str "v:" doc-id ":" (name attr)))

(defn- bytes->str
  ^String [^bytes bs]
  (.encodeToString (Base64/getEncoder) bs))

(defn- str->bytes
  [^String s]
  (.decode (Base64/getDecoder) s))

(defn put-field-posting! [store field term doc->tf]
  (lsm/put store (fterm-key field term) (bytes->str (codec/encode-posting doc->tf))))

(defn get-field-posting [store field term]
  (when-let [s (lsm/get store (fterm-key field term))]
    (codec/decode-posting (str->bytes s))))

(defn put-word!
  [store word doc->tf]
  (lsm/put store (word-key word) (bytes->str (codec/encode-posting doc->tf))))

(defn get-word
  [store word]
  (when-let [s (lsm/get store (word-key word))]
    (codec/decode-posting (str->bytes s))))

(defn put-doc-value!
  [store doc-id attr value]
  (lsm/put store (vkey doc-id attr) (str value)))

(defn get-doc-value
  [store doc-id attr]
  (lsm/get store (vkey doc-id attr)))

(defn put-field-doc!
  [store field doc-id text]
  (lsm/put store (fdoc-key field doc-id) text))

(defn get-field-doc
  [store field doc-id]
  (lsm/get store (fdoc-key field doc-id)))

(defn get-next-id
  [store]
  (if-let [s (lsm/get store next-id-key)]
    (Long/parseLong s)
    0))

(defn set-next-id!
  [store n]
  (lsm/put store next-id-key (str n)))

(defn put-field-length!
  [store field doc-id len]
  (lsm/put store (flen-key field doc-id) (str len)))

(defn get-field-length
  [store field doc-id]
  (if-let [s (lsm/get store (flen-key field doc-id))]
    (Long/parseLong s)
    0))

(defn set-field-total!
  [store field n]
  (lsm/put store (ftotal-key field) (str n)))

(defn get-field-total
  [store field]
  (if-let [s (lsm/get store (ftotal-key field))]
    (Long/parseLong s)
    0))

(defn doc-count
  [store]
  (get-next-id store))

(defn field-avg-doc-length [store field]
  (let [n (doc-count store)]
    (if (zero? n)
      0.0
      (/ (double (get-field-total store field)) n))))

(defn persist-index! [store index]
  (doseq [[fname f-idx] (:fields index)]
    (doseq [[term m] (:postings f-idx)]
      (put-field-posting! store fname term m))
    (doseq [[id len] (:doc-lengths f-idx)]
      (put-field-length! store fname id len))
    (set-field-total! store fname (reduce + 0 (vals (:doc-lengths f-idx)))))
  (doseq [[word m] (:words index)]
    (put-word! store word m))
  (doseq [[id fields] (:docs index)]
    (doseq [[fname text] fields]
      (put-field-doc! store fname id text)))
  (doseq [[id attrs] (:doc-values index)]
    (doseq [[attr value] attrs]
      (put-doc-value! store id attr value)))
  (set-next-id! store (:next-id index))
  store)

(defn scan-words [store prefix]
  (let [start (word-key prefix)]
    (->> (lsm/scan store start nil)
         (take-while (fn [[k _]] (.startsWith ^String k start)))
         (map (fn [[k v]] [(subs k (count "w:")) (codec/decode-posting (str->bytes v))])))))

(defn all-words [store]
  (->> (lsm/scan store "w:" "x")
       (map (fn [[k _]] (subs k (count "w:"))))))

(defn get-posting
  [store term]
  (get-field-posting store idx/default-field term))
(defn get-doc
  [store doc-id]
  (get-field-doc store idx/default-field doc-id))
(defn get-doc-length
  [store doc-id]
  (get-field-length store idx/default-field doc-id))
(defn avg-doc-length
  [store]
  (field-avg-doc-length store idx/default-field))
(defn get-total-len
  [store]
  (get-field-total store idx/default-field))