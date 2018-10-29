## Dhall: Haskellの新たなキラーアプリ

1. 設定ファイルの問題
1. Dhall のモチベーション
1. 導入方法
    1. dhall-to-json/dhall-to-yaml
    1. dhall コマンド
1. Kubernetes の Yaml を書いてみる
    1. Kubernetes の wall of Yaml
    1. Kubernetes Yaml のサンプル
    1. Dhall ver.1
    1. Dhall ver.1 を Yaml に変換
    1. Dhall で Kubernetes Yaml の型を定義
    1. Union を文字列に戻す
    1. Dhall ver.2
    1. デフォルト値などを自由に定義する
1. Dhall 関連ツールの紹介
    1. dhall サブコマンド群
    1. アプリケーション
    1. 言語バインディング
1. Haskell から Dhall を使う（時間が押していたら飛ばす）
    1. Haskell で Dhall ファイルを読む
    1. Haskell で Dhall を拡張する
1. まとめ

## 並列並行言語Haskell

1. 参考書: Haskell による並列・並行プログラミング
1. 並列・並行の動向
    1. CPU性能トレンド
    1. お求めやすくなったメニーコア
    1. 並行並列言語の台頭
1. 並列・並行と Haskell
    1. Haskell の並列・並行の歴史
    1. 並列・並行のためのよい性質
1. 並列・並行(・分散)の意味
    1. 目的による分類
    1. スレッドの有無による分類
    1. 分散
1. Haskell における並列並行のプリミティブ
    1. 軽量スレッドと評価戦略
1. 軽量スレッドを明示的に使う
    1. 軽量スレッドの作成: acync (forkIO)
    1. 軽量スレッド間の通信
    1. A Tour of Go in Haskell の宣伝
1. コードの評価順序を改変する
    1. まっすぐな評価を並列にする
    1. 評価戦略を分離する
1. より高度なツール
    1. 自動的な並列化 (Parモナド, Haxl)
    1. 行列計算の並列化 (repa, accelerate)
    1. 分散プログラミング (distributed-process a.k.a. Cloud Hakell)
    1. 軽量スレッドのプロファイリング (ThreadScope)
1. まとめ
1. おまけ: Haskell の並列並行関連のニュース (時間が押していたら飛ばす)
    1. ApplicativeDo (GHC 8.0)
    1. Facebook での成果: -qn オプション (GHC 8.2)
    1. GHC の NUMA サポート (GHC 8.2)
    1. 暗号通貨 Cardano は Cloud Haskell を使っている?
