(ns my-text-search.fuzzy-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.fuzzy :as fz]))

(deftest levenshtein-basic
  (is (= 0 (fz/levenshtein "" "")))
  (is (= 3 (fz/levenshtein "" "abc")))
  (is (= 0 (fz/levenshtein "abc" "abc")))
  (is (= 3 (fz/levenshtein "kitten" "sitting")))
  (is (= 2 (fz/levenshtein "flaw" "lawn"))))

(deftest levenshtein-japanese-edits
  (is (= 1 (fz/levenshtein "日本酒" "日木酒")))
  (is (= 1 (fz/levenshtein "日本酒" "日本")))
  (is (= 1 (fz/levenshtein "日本酒" "日本酒造")))
  (is (= 3 (fz/levenshtein "日本酒" "焼酎"))))

(deftest within-agrees-with-exact-distance
  (let [ws ["日本酒" "日木酒" "日本" "日本酒造" "焼酎" "" "abc" "abd"]]
    (doseq [a ws, b ws, k [0 1 2 3]]
      (is (= (<= (fz/levenshtein a b) k) (fz/within? a b k))
          (str a "/" b "/k=" k)))))

(deftest within-length-pruning
  (is (false? (fz/within? "日本酒" "焼酎" 0)))
  (is (false? (fz/within? "日本酒" "日本酒造造造" 2)))
  (is (true?  (fz/within? "日本酒" "日本酒造" 1))))

(deftest fuzzy-terms-collects-within-k
  (let [words ["日本酒" "日木酒" "日本酒造" "日本語" "焼酎" "日本"]]
    (is (= #{"日本酒"} (set (fz/fuzzy-terms words "日本酒" 0))))
    (is (= #{"日本酒" "日木酒" "日本酒造" "日本語" "日本"}
           (set (fz/fuzzy-terms words "日本酒" 1))))
    (is (= (set (fz/fuzzy-terms-naive words "日本酒" 1))
           (set (fz/fuzzy-terms words "日本酒" 1))))))

(deftest fuzzy-search-ors-candidate-word-postings
  (let [words ["日本酒" "日木酒" "日本語" "焼酎"]
        wpf   {"日本酒" [0 2], "日木酒" [1], "日本語" [4], "焼酎" [3]}]
    (is (= [0 1 2 4] (vec (fz/fuzzy-search words #(get wpf %) "日本酒" 1))))
    (is (= [0 2]     (vec (fz/fuzzy-search words #(get wpf %) "日本酒" 0))))
    (is (= [0 1 2]   (vec (fz/fuzzy-search words #(get wpf %) "日木酒" 1))))))
