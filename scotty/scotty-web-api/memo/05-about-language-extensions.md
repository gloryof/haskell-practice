## 言語拡張

### OverloadedStrings

http://d.hatena.ne.jp/sirocco/20130415/1365974719  
によると`String`を`Text`/`ByteString`として扱ってくれるらしい。

ただ、`Text`/`ByteString`として扱うことになんのメリットがあるのか？  

http://bicycle1885.hatenablog.com/entry/2012/12/24/234707

`String`だとただの配列なので効率が悪い。  
`Text`/`ByteString`だと効率よく扱える。  
`Text`は`ByteString`より効率は悪いけど、マルチバイト文字が扱える。  
ってかんじかな？

#### ためしてみる

```hs
register u = do
  modify $ trace (show test) addUser u
  st <- get
  return $ fromJust $ getUserId $ fromJust $ recentry st
```

というコードにする。  
```hs
test :: String
test = "ほげ"
```
にして実行すると`"\12411\12370"`という出力になった。  

`test :: ByteString`に変えてみる。  
（cabalファイルやimportを変えたけど割愛）  
`"{R"`という出力。

`test :: Text`に変えてみる。  
`"\12411\12370"`  
`show`では変わらないらしい・・・

#### ためしてみる part2

srcディレクトリ側の`UserCase.User`に

```hs
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T

test :: IO ()
test = do
  test <- return $ "ほげ"
  putStrLn test
  B8.putStrLn $ B8.pack test
  T.putStrLn $ T.pack test
```

を追加して、`stack ghci`で実行。  
```
ほげ
{R
ほげ
```

`String`と`Text`は日本語で出てくれた。  

#### まとめ

Stringでやると効率が悪いので、`Text` / `ByteString`でやろう。  
でも、いちいち変換が面倒だから`OverloadedStrings`を使おう。  

って感じかな？

### GeneralizedNewtypeDeriving

http://d.hatena.ne.jp/D_Rascal/20111223/1324646018  
によると`newtype`宣言したものに対して簡単に`deriving`できるっぽい。  

http://qiita.com/hiratara/items/888bdae1455fd7c4a5f0  
メインとなるテーマは`GeneralizedNewtypeDeriving`じゃないけど参考にさせていただいた。  

```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)
```
というのは`StaetT`周りの実装を使いまわすということかな？  

`Functor`/`Applicative`/`Monad`/`MonadTrans`までは`StateT`のソースコード内で実装されている。  
`MonadThrow`/`MonadCatch`は`Control.Monad.Catch`側で実装されている。  

ということは全て`StateT`の実装が使い回されている。  

`GeneralizedNewtypeDeriving`を外すと・・・
```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:34:13: error:
    • Can't make a derived instance of
        ‘Functor (UserMockRepository m)’:
        You need DeriveFunctor to derive an instance for this class
        Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
    • In the newtype declaration for ‘UserMockRepository’           
```
こんな感じでエラーになる。  

### StandaloneDeriving

http://qiita.com/hyone/items/a47b905936e55147fe59  
によると`deriving`は型定義の時しかできないけど、  
`StandaloneDeriving`を使うと後から`deriving`できるらしい。  

```hs
deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
deriving instance (Functor m, MonadCatch m, MonadIO m) => MonadIO (UserMockRepository m)
```
`newtype`宣言とはカインドが違うから後で`deriving`しているっぽい。

### MultiParamTypeClasses

http://rf0444.hatenablog.jp/entry/20120513/1336883141  
によると型パラメータを複数持てるらしい。  

`MultiParamTypeClasses`を外すと
```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:45:48: error:
    • Illegal instance declaration for
        ‘MonadState UserMockState (UserMockRepository m)’
        (Only one type can be given in an instance head.
         Use MultiParamTypeClasses if you want to allow more, or zero.)
    • In the stand-alone deriving instance for
        ‘(Functor m, MonadCatch m) =>
         MonadState UserMockState (UserMockRepository m)’
```
というコンパイルエラーになった。  

https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Class.html  
を見ると`Portability	non-portable (multi-param classes, functional dependencies)`ってなっているので、
`MonadState`を使う場合はこの拡張が必要？  

https://github.com/yaakaito/NLTQuickCheck/wiki/yaakaito-%E3%81%AE%E3%81%9F%E3%82%81%E3%81%AE-Haskell-%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA%E3%81%AE%E8%AA%AD%E3%81%BF%E6%96%B9

Portabilityの説明を見ると合ってそう。
