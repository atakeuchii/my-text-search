# my-text-search

Clojure で実装した、転置インデックスベースのミニ全文検索エンジン。
Elasticsearch / Lucene のコア原理（転置インデックス・BM25・positional index・
doc values・検索品質評価）を、自作の LSM-tree ストレージエンジン
[my-storage](https://github.com/atakeuchii/my-storage) を永続化層として一から実装したもの。

## 特徴

- N-gram（bi-gram）トークナイザ（NFKC 正規化つき）
- 4種のマッチング: Boolean（AND/OR）/ ワイルドカード（前方一致）/ ファジー（編集距離）/ フレーズ（隣接）
- BM25 スコアリング（TF 飽和 k1・文書長正規化 b）とフィールド別 boost
- ハイライト（スニペット生成）とファセット集計（doc values）
- precision / recall による検索品質の評価
- delta + varint による圧縮ポスティングを LSM-tree に永続化

## アーキテクチャ

    文書 {フィールド→テキスト, 属性→値}
      │
      ▼
    tokenizer  … 正規化(NFKC) + N-gram / セグメント分割
      │
      ▼
    index      … 転置インデックス {フィールド → {term → {doc → [位置]}}}
      │            + 語辞書(:words) + doc values(:doc-values)
      ▼
    codec      … ポスティングを delta + varint で符号化（flags で TF/位置を拡張）
      │
      ▼
    store      … my-storage(LSM-tree) へ永続化
                 key: t:<field>:<term> / w:<word> / d:<doc>:<field>
                      / l:<field>:<doc> / v:<doc>:<attr> / m:...

    検索側:
    query   … Boolean(積/和集合) / ワイルドカード / ファジー / フレーズ
    score   … TF-IDF → BM25 → フィールド boost 合成
    highlight … 正規化本文でマッチを再特定してスニペット
    facet   … doc values を属性値でグルーピングして集計
      │
      ▼
    search  … 記法振り分け + ランキング + スニペット + ファセット
      │
      ▼
    core    … build! / open / search / close（公開API）

## 使い方

```clojure
(require '[my-text-search.core :as ts])

;; 構築（fsync はまとめる設定で高速化）
(ts/build! "my-index"
  [{:fields {:name "獺祭" :description "山口 華やか 日本酒"} :attrs {:region "山口" :type "大吟醸"}}
   {:fields {:name "久保田" :description "新潟 淡麗 日本酒"} :attrs {:region "新潟" :type "吟醸"}}]
  {:wal-fsync 1000})

;; 検索（ランキング + ファセット + スニペット）
(def db (ts/open "my-index"))
(ts/search db "日本酒"
  :fields {:description 1.0 :name 2.0}   ; フィールド別 boost
  :facet-attrs [:region :type]           ; 集計する属性
  :snippet-field :description)           ; スニペットを作るフィールド
;; => {:results [{:doc-id 1 :score 0.71 :snippet "新潟 淡麗 《日本酒》"} ...]
;;     :facets {:region {"新潟" 1 "山口" 1} :type {...}}}
(ts/close db)
```

検索記法: 末尾 `*` = 前方一致（`獺祭*`）、末尾 `~` = ファジー（`日本酒~`）、それ以外は Boolean。

## 主要な設計判断

- **正規化の不変条件**: index 時と query 時で必ず同じ `normalize`（NFKC + ロケール非依存の小文字化）を通す。片方だけ正規化するとトークンが一致せずヒットしないため。全機能を貫く前提。
- **ポスティングは `{doc → [位置]}` に一本化**: TF は位置数から導出。codec は flags バイトで TF セクション（bit0）・位置セクション（bit1）を後付け拡張できる設計にし、doc-id 列のフォーマットを変えずに TF→位置と機能追加した。
- **text フィールドと doc values の分離**: 検索対象（トークナイズ）はフィールド、集計・完全一致対象（値のまま）は属性。Elasticsearch の text 型 / keyword 型の区別に対応。
- **マッチングとスコアリングの直交**: 「どの文書が当たるか」（4種のマッチング）と「どう並べるか」（BM25 + boost）を分離。取得元を関数（posting-fn 等）で抽象化し、store／オンメモリ双方で同じロジックが動く。
- **語辞書（:words）はフィールド横断**: ワイルドカード・ファジーは全フィールドを対象に候補を集め、boost でフィールド重要度を反映する。

## Elasticsearch との対応

| 機能 | Elasticsearch | 本実装 |
|---|---|---|
| AND/OR 検索 | bool query | query/search |
| ワイルドカード | wildcard query | query/wildcard-search |
| ファジー | fuzzy query | fuzzy/fuzzy-search |
| フレーズ | match_phrase | query/phrase-search |
| スコアリング | BM25 (_score) | score/bm25-* |
| フィールド boost | fields^n | score/bm25-multi-field-* |
| ハイライト | highlight | highlight/snippet |
| ファセット | aggregations | facet/facet-counts |
| 品質評価 | rank_eval | eval/evaluate |
| 分散・レプリケーション | sharding/replica | 対象外 |

## ベンチマーク

`lein with-profile +bench run -m my-text-search.search-bench <文書数>`

文書数 2000 での実測（環境依存）:

| 項目 | 値 |
|---|---|
| 索引構築（オンメモリ） | 169.7 ms |
| 永続化（`:wal-fsync 1000`） | 471.6 ms |
| インデックスサイズ | 330.3 KB |
| Boolean 検索 | 1.418 ms/query |
| ワイルドカード | 1.277 ms/query |
| ファジー | 1.360 ms/query |
| フレーズ | 3.171 ms/query |

観察:
- **フレーズ検索の最適化**: 当初、候補文書ごとにポスティングを LSM から再取得していたため約 320 ms/query と極端に遅かった。ポスティングを term ごとに1回だけ取得してキャッシュする修正で約 1.6 ms/query（約 200 倍）に改善。
- **永続化は fsync が支配**: put ごとに fsync（既定 always）だと 2000 文書で約 24 秒。`:wal-fsync 1000`（グループコミット）で約 470 ms に短縮。耐久性とスループットのトレードオフが数値で確認できる。

## 名前空間

- `tokenizer` … 正規化 + N-gram / セグメント
- `index` … 転置インデックス構築（オンメモリ）
- `codec` … ポスティングの符号化（delta + varint + 位置）
- `store` … LSM-tree 永続化（my-storage）
- `query` … Boolean / ワイルドカード / ファジー / フレーズ
- `score` … TF-IDF / BM25 / フィールド boost
- `highlight` … スニペット生成
- `facet` … ファセット集計
- `eval` … precision / recall 評価
- `search` … 記法振り分け + ランキング + スニペット + ファセット
- `core` … 公開 API

## 今後の課題

- BM25 パラメータ（k1/b）の自動チューニング（`eval/sweep` を k1/b に拡張）
- ファジー × ワイルドカードの合成検索（区切りの少ない日本語向け）
- ハイライトの原文オフセット対応（索引に char offset を持たせる Lucene 方式）
- 再起動後の文書追記（load-modify-flush または read-modify-write）
- 実データ投入と、Go / Rust での再実装
