## Dhall: Haskellの新たなキラーアプリ

1. 

## 並列並行言語Haskell

1. 参考書: Haskell による並列・並行プログラミング
1. 並列・並行の動向
    1. シングルコア性能の停滞
    1. お求めやすくなったメニーコア
    1. 並行並列言語の台頭
1. 並列・並行と Haskell
    1. Haskell の並列・並行の歴史
    1. 並列・並行のためのよい性質
1. 並列・並行(・分散)の意味
    1. 目的による分類
    1. スレッドの有無による分類
    1. 分散
1. Haskell における並列並行の道具
    1. 軽量スレッドと評価戦略
1. 軽量スレッドによる並列と並行
    1. 軽量スレッドの作成: async (forkIO)
    1. MVar によるスレッド間協調
    1. ソフトウェアトランザクショナルメモリ
    1. MVar と STM の違い
    1. 非同期例外
1. 評価戦略: 評価をコントロールする
    1. ふつうの遅延評価
    1. rseq/rpar による評価のコントロール
    1. 評価戦略の分離
1. 並列プロファイリング
    1. ThreadScope
1. 紹介しなかった道具
    1. データフロー並列・パイプライン並列
    1. データ並列
    1. GPU プログラミング
    1. distributed-process (Cloud Haskell)
1. まとめ
1. おまけ: Haskell の並列並行関連のニュース
    1. GHC の NUMA サポート
    1. Facebook での成果: -qn オプション
    1. 暗号通貨 Cardano は Cloud Haskell を使っている?