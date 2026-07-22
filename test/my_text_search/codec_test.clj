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
  (doseq [s [(sorted-set 0) (sorted-set 0 3 5 12) (sorted-set 1 2 3 4 5) (sorted-set 7 100 1000 100000) (sorted-set)]]
    (is (= s (codec/decode-posting (codec/encode-posting s))))))

(deftest posting-known-bytes
  (is (= [0 4 0 3 2 7]
         (vec (codec/encode-posting (sorted-set 0 3 5 12))))))

(deftest delta-keeps-bytes-small
  (is (= 5 (alength (codec/encode-posting(sorted-set 100 200 300))))))

(deftest first-byte-is-flags-zero
  (is (zero? (first (codec/encode-posting (sorted-set 5 9))))))
