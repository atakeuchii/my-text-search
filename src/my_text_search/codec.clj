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
  "ソート済み {文書ID -> [位置(昇順)]} をバイト列へ。入力は doc-id 昇順であること。
   [flags(1B)=2][doc-count][delta-doc-id...][各doc: [pos-count][delta-pos...]]
   flags bit1=1: 位置セクションあり。tf は位置数から導出する。"
  ^bytes [doc->pos]
  (let [out (ByteArrayOutputStream.)
        entries (seq doc->pos)
        ids (map key entries)
        poss (map val entries)]
    (.write out (int 2))
    (write-uvarint! out (count entries))
    (loop [prev 0
           es ids]
      (when (seq es)
        (write-uvarint! out (- (long (first es)) prev))
        (recur (long (first es)) (rest es))))
    (doseq [ps poss]
      (write-uvarint! out (count ps))
      (loop [prev 0
             es (seq ps)]
        (when (seq es)
          (write-uvarint! out (- (long (first es)) prev))
          (recur (long (first es)) (rest es)))))
    (.toByteArray out)))

(defn decode-posting
  "バイト列を {文書ID -> [位置]}(sorted-map) に復元する。"
  [^bytes bs]
  (let [buf (ByteBuffer/wrap bs)
        _flags (bit-and (long (.get buf)) 0xff)
        n (read-uvarint buf)
        ids (loop [i 0
                   prev 0
                   acc (transient [])]
              (if (< i n)
                (let [id (+ prev (read-uvarint buf))]
                  (recur (inc i) id (conj! acc id)))
                (persistent! acc)))
        poss (loop [i 0
                    acc (transient [])]
               (if (< i n)
                 (let [pc (read-uvarint buf)
                       ps (loop [j 0
                                 prev 0
                                 pacc (transient [])]
                            (if (< j pc)
                              (let [p (+ prev (read-uvarint buf))]
                                (recur (inc j) p (conj! pacc p)))
                              (persistent! pacc)))]
                   (recur (inc i) (conj! acc ps)))
                 (persistent! acc)))]
    (into (sorted-map) (map vector ids poss))))
