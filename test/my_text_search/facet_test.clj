(ns my-text-search.facet-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.facet :as facet]))

(deftest facet-counts-groups-and-counts
  (let [vf {0 "山口" 1 "新潟" 2 "新潟" 3 "山口"}
        f  #(get vf %)]
    (is (= {"山口" 2 "新潟" 2} (facet/facet-counts f [0 1 2 3])))
    (is (= {"新潟" 2} (facet/facet-counts f [1 2])))
    (is (= {"山口" 1} (facet/facet-counts f [0 99])))))

(deftest facets-multiple-attributes
  (let [region {0 "山口" 1 "新潟"} type {0 "大吟醸" 1 "吟醸"}]
    (is (= {:region {"山口" 1 "新潟" 1} :type {"大吟醸" 1 "吟醸" 1}}
           (facet/facets {:region #(get region %) :type #(get type %)} [0 1])))))

(deftest sorted-counts-desc
  (is (= [["新潟" 3] ["山口" 1]]
         (facet/sorted-counts {"山口" 1 "新潟" 3}))))
