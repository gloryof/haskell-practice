## モナドの戻り値の型

```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st
```

この各行の戻り値の型がよくわからない。  
`modify`は`modify :: Monad m => (s -> s) -> StateT s m ()`という定義なので、  
値を受け取っていれば`UserMockRepository (StateT UserMockStaet m ()) UserId`と予想。  

`get`は`get :: Monad m => StateT s m s`なので、  
`UserMockRepository (StateT UserMockState m UserMockState)`と予想。  

特に不明なのは最後の行。  
値変数`st`は`UserMockState`型になっている。  
`StateT UserMockState m UserMockState`ではないのか？  
この辺は`StateT`のモナドトランスフォーマーによる動きっぽいが。  

### Maybeで試してみる

```hs
runMaybe :: Maybe a -> a
runMaybe v = fromJust v
```
という`Maybe`で実行するための関数を用意する。  

hspecのコードはこんな感じ。
```hs
specMock :: Spec
specMock = do
  describe "mock" $ do
    it "What type is this?" $ do
      res <- return $ runMaybe (SUT.save newUser)
      res `shouldBe` DU.UserId 3
```
若干ここも意味がわかっていないが・・・

#### 一番単純なパターン

ソースコードはこんな感じ。
```hs
instance UserRepository Maybe where
  register u = do
    hoge  <- return 3
    Just $ UserId $ trace (show hoge) hoge
```

このdoの中では`Maybe`なので`hoge <- return 3`は`Maybe Int`。  
`Just $ UserId $ trace (show hoge) hoge`で、  
値変数`hoge`は`Maybe`から解かれて、`Int`になる。  

型に解きほどいて行く。

`Just $ UserId $ trace (show hoge) hoge`  
は  
`Just $ UserId $ trace (String) Int`  
となる。  
そして、`trace`は`trace :: String -> a -> a`なので、  
`Just $ UserId Int`となり、`Just UserId`になる。

改めてみると`return`の動きがよくわかってない。  

#### return

`return`の定義は型クラス`Monad`で行われている。  
`Maybe`ではデフォルト実装が使用されている。
```hs
class Applicative m => Monad m where
    return      :: a -> m a
    return      = pure
```

`pure`の定義は型クラス`Applicative`で行われていていて、  
`Maybe`の中では下記のようになっている。
```hs
instance Applicative Maybe where
    pure = Just
```

つまり、`hoge <- return 3`は`hoge <- Just 3`ということになる。

ちなみに`Maybe`のデータ型の定義は下記のような感じ。
```hs
data  Maybe a  =  Nothing | Just a
  deriving (Eq, Ord)
```

`return`は任意の型を受け取り、モナドで包むということになる。  
ちなみにリストモナドは下記ような感じ。

```hs
instance Applicative [] where
    pure x    = [x]
```

#### 束縛

https://twitter.com/uebayasi/status/899999084764778501

Twitterでアドバイスをいただいたのでその辺を調べてみる。  

`>>=`は型クラス`Monad`で定義されている。  

```hs
(>>=)  :: forall a b. m a -> (a -> m b) -> m b
```
ということで、
```hs
instance UserRepository Maybe where
  register u = do
    hoge  <- return 3
    Just $ UserId $ trace (show hoge) hoge
```
を置き換えてみる。  

```hs
register u =  (Just 3) >>= (\hoge -> Just $ UserId $ trace (show hoge) hoge)
```
かなりスッキリした。  

```hs
(Just 3) >>= (\hoge -> Just $ UserId $ trace (show hoge) hoge)
```
を一つずつ解きほどいて行く。  

まずは素直に型を当てはめて行く。
```hs
(Maybe Int) >>= (Int -> Maybe UserId)
```
`>>=`中置っぽいので
```hs
(>>=)  :: Maybe Int -> (Int  -> Maybe UserId) -> Maybe UserId
```
になる。

`hoge  <- return 3`というのは`Just 3 >>= (\hoge -> ...)`と同じ？  
`Just $ UserId $ trace (show hoge) hoge`というのはどちらも同じ。

`>>=`は`m a -> (a -> m b) -> m b`なので、  
あるモナドに包まれている状態で別の型（a -> b）に変換できる。

`Maybe`の型定義は`data  Maybe a  =  Nothing | Just a`なので、  
`Just a`は型変数`a`の値が渡されるのでイメージしやすいけど、  
`Nothing`には型変数（というか）包まれた値を持たないからダメなのでは？

```hs
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing
```

と思ったら、パターンマッチングで弾いている。


### まだわからない・・・

モナドの基本的な動きが理解できたので最初のコードに戻ってみる。

```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st
```

do記法を使わないとこんな感じ。
```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u =
    (modify $ addUser u)
    >>= (\_ -> get)
    >>= (\st -> return $ fromJust $ getUserId $ fromJust $ recentry st)
```
`modify :: Monad m => (s -> s) -> StateT s m ()`なので、  
一行目の型変数は`a -> UserMockRepository (StateT UserMockState m ()) a`となっている？  

`get :: Monad m => StateT s m s`なので、  
2行目は`a -> UserMockRepository (StateT UserMockState m UserMockState) a`となっている？  

この辺はStateモナドで調べる。
