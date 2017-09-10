## deriving

そもそも`deriving`とは何だろうか？  

本来、型クラスで必要な関数を実装するべきなのだが、  
`derving`を使うことにより自動的に導出してくれる。  

自分で独自に定義した型も`deriving`できるのか？
```hs
data Test = Test Int deriving (Eq2, Eq3)

class (Eq a) => Eq2 a where
  equals   ::  a -> a -> Bool
  equals x y  =  (x == y)

class (Eq a) => Eq3 a where
  diff   ::  a -> a -> Bool

```
コンパイルしてみると・・・
```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:31:32: error:
    • Can't make a derived instance of ‘Eq2 Test’:
        ‘Eq2’ is not a standard derivable class (Eq, Show, etc.)
        Try enabling DeriveAnyClass
    • In the data declaration for ‘Test’
```
ダメだった。  

`a standard derivable`となっているので、`deriving`できる型クラスとできない型クラスがあるらしい。  

`DeriveAnyClass`っていう拡張を使えば独自の型クラスでも`deriving`できる。  
`{-# LANGUAGE DeriveAnyClass #-}`をつけるとコンパイルが通る。


https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-instances-of-extra-classes-data-etc

ここを見ると`deriving`できるのは  
`Eq`, `Ord`, `Enum`, `Ix`, `Bounded`, `Read`, `Show`だけの様子。
