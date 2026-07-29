(ns my-text-search.index
  (:require [my-text-search.tokenizer :as tok]))

(def default-field :text)

(defn empty-index
  []
  {:fields (sorted-map)
   :words (sorted-map)
   :docs {}
   :next-id 0})

(defn- positions-by-token
  [tokens]
  (reduce (fn [m [pos t]] (update m t (fnil conj []) pos))
          {} (map-indexed vector tokens)))

(defn- add-postings 
  [m tok->pos doc-id]
  (reduce (fn [acc [t poss]]
            (update acc t (fnil assoc (sorted-map)) doc-id poss))
          m tok->pos))

(defn- add-field-positions 
  [f-idx tok->pos doc-id dl]
  (-> f-idx
      (update :postings (fnil add-postings (sorted-map)) tok->pos doc-id)
      (update :doc-lengths (fnil assoc {}) doc-id dl)))

;; (defn- add-field-freqs
;;   [f-idx freqs doc-id dl]
;;   (-> f-idx
;;       (update :postings (fn [p] (reduce (fn [m [t cnt]]
;;                                           (update m t (fnil assoc (sorted-map)) doc-id cnt))
;;                                         (or p (sorted-map))
;;                                         freqs)))
;;       (update :doc-lengths (fnil assoc {}) doc-id dl)))

;; (defn- add-freqs
;;   [m freqs doc-id]
;;   (reduce (fn [acc [k cnt]]
;;             (update acc k (fnil assoc (sorted-map)) doc-id cnt))
;;           m freqs))

(defn add-document
  [index fields]
  (let [doc-id (:next-id index)]
    (-> (reduce
         (fn [idx [fname text]]
           (let [tokens (tok/tokenize text)
                 tok->pos (positions-by-token tokens)
                 dl (count tokens)
                 seg->pos (positions-by-token (tok/segments text))]
             (-> idx
                 (update-in [:fields fname]
                            (fnil add-field-positions {:postings (sorted-map) :doc-lengths {}})
                            tok->pos doc-id dl)
                 (update :words add-postings seg->pos doc-id))))
         index
         fields)
        (update :docs assoc doc-id fields)
        (assoc :next-id (inc doc-id)))))

(defn add-text
  [index text]
  (add-document index {default-field text}))

(defn field-posting
  [index fname term]
  (get-in index [:fields fname :postings term] (sorted-map)))

(defn field-doc-length
  [index fname doc-id]
  (get-in index [:fields fname :doc-lengths doc-id] 0))

(defn field-avg-doc-length
  [index fname]
  (let [ls (vals (get-in index [:fields fname :doc-lengths]))]
    (if (empty? ls)
      0.0
      (/ (double (reduce + 0 ls)) (count ls)))))

(defn fields
  [index]
  (keys (:fields index)))

(defn posting
  [index term]
  (field-posting index default-field term))
(defn doc-length
  [index doc-id]
  (field-doc-length index default-field doc-id))
(defn avg-doc-length
  [index]
  (field-avg-doc-length index default-field))
(defn word-posting
  [index word]
  (get-in index [:words word] (sorted-map)))
(defn doc-text
  ([index doc-id] (get-in index [:docs doc-id default-field]))
  ([index doc-id fname] (get-in index [:docs doc-id fname])))

(defn words-with-prefix
  [index prefix]
  (->> (subseq (:words index) >= prefix)
       (take-while (fn [[w _]] (.startsWith ^String w prefix)))
       (map (fn [[w ids]] [w ids]))))

(defn stats
  [index]
  {:docs (count (:docs index))
   :fields (into {} (for [[f fi] (:fields index)] [f (count (:postings fi))]))
   :words (count (:words index))})
