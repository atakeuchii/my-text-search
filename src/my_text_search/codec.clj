(ns my-text-search.codec
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer]))

(defn write-uvarint!
  "非負整数 n を unsigned LEB128 varint で out に書く。
   下位7bitずつ書き、まだ続くなら最上位bit(継続フラグ)を立てる。"
  [^ByteArrayOutputStream out ^long n]
  (loop [n n]
    (if (< n 0x80)
      (.write out (int n))
      (do (.write out (int (bit-or (bit-and n 0x7f) 0x80)))
          (recur (unsigned-bit-shift-right n 7))))))

(defn read-uvarint
  "buf の現在位置から varint を1つ読んで値を返す（buf の位置は進む）。"
  ^long [^ByteBuffer buf]
  (loop [shift 0
         result 0]
    (let [b (bit-and (long (.get buf)) 0xff)
          result (bit-or result (bit-shift-left (bit-and b 0x7f) shift))]
      (if (zero? (bit-and b 0x80))
        result
        (recur (+ shift 7) result)))))

(defn uvarint->bytes ^bytes [^long n]
  (let [out (ByteArrayOutputStream.)]
    (write-uvarint! out n)
    (.toByteArray out)))

(defn encode-posting
  "ソート済み文書ID集合をバイト列へ。
   [flags(1B)=0][doc-count varint][delta-doc-id varint...]
   flags は将来 TF/位置セクションの有無を示すための余白。"
  ^bytes [doc-ids]
  (let [out (ByteArrayOutputStream.)
        ids (vec doc-ids)]
    (.write out (int 0))
    (write-uvarint! out (count ids))
    (loop [prev 0
           es (seq ids)]
      (when-let [id (first es)]
        (write-uvarint! out (- id prev))
        (recur id (next es))))
    (.toByteArray out)))

(defn decode-posting
  "encode-posting のバイト列を (sorted-set 文書ID...) に復元する。"
  [^bytes bs]
  (let [buf (ByteBuffer/wrap bs)
        flags (bit-and (long (.get buf)) 0xff)
        n (read-uvarint buf)]
    (loop [i 0
           prev 0
           acc (transient [])]
      (if (< i n)
        (let [id (+ prev (read-uvarint buf))]
          (recur (inc i) id (conj! acc id)))
        (into (sorted-set) (persistent! acc))))))
