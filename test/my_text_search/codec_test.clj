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

(deftest posting-roundtrip
  (doseq [m [(sorted-map 0 1)
             (sorted-map 0 3 3 1 5 2 12 1)
             (sorted-map 1 1 2 1 3 1 4 1 5 1)
             (sorted-map 7 10 100 1 1000 5 100000 2)
             (sorted-map)]]
    (is (= m (codec/decode-posting (codec/encode-posting m))))))

(deftest posting-tf-over-varint-boundary
  ;; TF も varint なので 127 を跨いでも壊れない
  (let [m (sorted-map 0 127 1 128 2 300)]
    (is (= m (codec/decode-posting (codec/encode-posting m))))))

(deftest posting-known-bytes
  ;; {0 3, 3 1, 5 2} -> flags1, count3, delta[0 3 2], tf[3 1 2]
  (is (= [1 3 0 3 2 3 1 2]
         (vec (codec/encode-posting (sorted-map 0 3 3 1 5 2))))))

(deftest delta-keeps-bytes-small
  ;; flags(1) + count(1) + delta[100 100 100](3) + tf[1 1 1](3) = 8
  ;; delta を取らなければ 300 が 2 バイトになり 9 バイトへ増える
  (is (= 8 (alength (codec/encode-posting (sorted-map 100 1 200 1 300 1))))))

(deftest first-byte-is-flags-with-tf-bit
  ;; TF セクションありなので bit0 が立つ
  (is (= 1 (first (codec/encode-posting (sorted-map 5 1 9 1))))))

(deftest posting-decodes-old-format-as-tf1
  ;; 旧形式(flags=0, TFなし)は全 tf=1 として読める（後方互換）
  (let [old (byte-array [0 3 0 3 2])]
    (is (= (sorted-map 0 1 3 1 5 1) (codec/decode-posting old)))))
