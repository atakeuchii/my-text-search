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
  "ソート済み {文書ID -> TF} をバイト列へ符号化する。入力は doc-id 昇順であること。
   [flags(1B)][doc-count varint][delta-doc-id varint...][tf varint...]
   flags bit0=1: TF セクションあり。"
  ^bytes [doc->tf]
  (let [out (ByteArrayOutputStream.)
        entries (seq doc->tf)
        ids (map key entries)
        tfs (map val entries)]
    (.write out (int 1))
    (write-uvarint! out (count entries))
    (loop [prev 0
           es ids]
      (when (seq es)
        (write-uvarint! out (- (long (first es)) prev))
        (recur (long (first es)) (rest es))))
    (doseq [tf tfs]
      (write-uvarint! out (long tf)))
    (.toByteArray out)))

(defn decode-posting
  "バイト列を {文書ID -> TF}(sorted-map) に復元する。
   flags の bit0 で TF セクションの有無を判定。旧形式(TFなし)は tf=1 とみなす。"
  [^bytes bs]
  (let [buf (ByteBuffer/wrap bs)
        flags (bit-and (long (.get buf)) 0xff)
        has-tf? (pos? (bit-and flags 1))
        n (read-uvarint buf)
        ids (loop [i 0
                   prev 0
                   acc (transient [])]
              (if (< i n)
                (let [id (+ prev (read-uvarint buf))]
                  (recur (inc i) id (conj! acc id)))
                (persistent! acc)))
        tfs (if has-tf?
              (loop [i 0
                     acc (transient [])]
                (if (< i n)
                  (recur (inc i) (conj! acc (read-uvarint buf)))
                  (persistent! acc)))
              (repeat n 1))]
     (into (sorted-map) (map vector ids tfs))))
