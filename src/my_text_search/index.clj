(ns my-text-search.index
  (:require [my-text-search.tokenizer :as tok]))

(def default-field :text)

(defn empty-index
  []
  {:fields (sorted-map)
   :words (sorted-map)
   :docs {}
   :next-id 0})

(defn- add-field-freqs
  [f-idx freqs doc-id dl]
  (-> f-idx
      (update :postings (fn [p] (reduce (fn [m [t cnt]]
                                          (update m t (fnil assoc (sorted-map)) doc-id cnt))
                                        (or p (sorted-map))
                                        freqs)))
      (update :doc-lengths (fnil assoc {}) doc-id dl)))

(defn- add-freqs
  [m freqs doc-id]
  (reduce (fn [acc [k cnt]]
            (update acc k (fnil assoc (sorted-map)) doc-id cnt))
          m freqs))

(defn add-document
  [index fields]
  (let [doc-id (:next-id index)]
    (-> (reduce
         (fn [idx [fname text]]
           (let [freqs (frequencies (tok/tokenize text))
                 words (frequencies (tok/segments text))
                 dl (reduce + 0 (vals freqs))]
             (-> idx
                 (update-in [:fields fname]
                            (fnil add-field-freqs
                                  {:postings (sorted-map) :doc-lengths {}})
                            freqs doc-id dl)
                 (update :words add-freqs words doc-id))))
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
   :words (count (:words index))}
  ;; (let [postings (:postings index)
  ;;       term-count (count postings)
  ;;       total (reduce + 0 (map count (vals postings)))]
  ;;   {:docs (count (:docs index))
  ;;    :terms term-count
  ;;    :words (count (:words index))
  ;;    :postings total
  ;;    :avg-posting (if (zero? term-count)
  ;;                   0.0
  ;;                   (double (/ total term-count)))})
  )
