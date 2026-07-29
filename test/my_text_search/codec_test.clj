(ns my-text-search.codec-test
  (:require [clojure.test :refer [deftest is]]
            [my-text-search.codec :as codec])
  (:import [java.nio ByteBuffer]))

(defn- read-uvarint-from [^bytes bs]
  (codec/read-uvarint (ByteBuffer/wrap bs)))

(deftest uvarint-byte-length
  ;; 小さい数ほど短い（delta を小さくする意義そのもの）
  (is (= 1 (alength (codec/uvarint->bytes 0))))
  (is (= 1 (alength (codec/uvarint->bytes 127))))   ; 7bit境界の内側
  (is (= 2 (alength (codec/uvarint->bytes 128))))   ; 境界を越えて2バイト
  (is (= 2 (alength (codec/uvarint->bytes 16383))))
  (is (= 3 (alength (codec/uvarint->bytes 16384)))))

(deftest uvarint-known-encoding
  ;; 代表値のバイト列を固定
  (is (= [-128 1]  (vec (codec/uvarint->bytes 128))))   ; 0x80 0x01（-128はbyteの0x80）
  (is (= [-84 2]   (vec (codec/uvarint->bytes 300)))))  ; 0xAC 0x02

(deftest uvarint-roundtrip
  (doseq [n [0 1 127 128 300 16383 16384 1000000 123456789]]
    (is (= n (read-uvarint-from (codec/uvarint->bytes n))))))

(deftest posting-positions-roundtrip
  (doseq [m [(sorted-map 0 [0 2] 3 [1])
             (sorted-map 0 [0])
             (sorted-map 0 [0 5 10] 7 [3] 100 [0 1 2 3])
             (sorted-map)]]
    (is (= m (codec/decode-posting (codec/encode-posting m))))))

(deftest posting-tf-over-varint-boundary
  ;; TF も varint なので 127 を跨いでも壊れない
  (let [m (sorted-map 0 [127] 1 [128] 2 [300])]
    (is (= m (codec/decode-posting (codec/encode-posting m))))))

(deftest posting-positions-known-bytes
  ;; {0 [0 2], 3 [1]} -> flags2, count2, doc-delta[0 3], doc0(cnt2,[0 2]), doc3(cnt1,[1])
  (is (= [2 2 0 3 2 0 2 1 1]
         (vec (codec/encode-posting (sorted-map 0 [0 2] 3 [1]))))))

(deftest delta-keeps-bytes-small
  ;; flags(1) + count(1) + delta[100 100 100](3) + tf[1 1 1](3) = 8
  ;; delta を取らなければ 300 が 2 バイトになり 9 バイトへ増える
  (is (= 11 (alength (codec/encode-posting (sorted-map 100 [0] 200 [0] 300 [0]))))))

(deftest first-byte-is-flags-with-tf-bit
  ;; TF セクションありなので bit0 が立つ
  (is (= 2 (first (codec/encode-posting (sorted-map 5 [0] 9 [0]))))))
