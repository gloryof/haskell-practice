## モックについて考察

いまいち理解できていないのでメモ。  
まとめというよりはどういうかことを考えたかなのでまとまってはいない。

参考にさせていただいたサイトは下記。  
* http://krdlab.hatenablog.com/entry/2015/11/03/122732
* http://straitwalk.hatenablog.com/entry/20120917/1347862238
* http://d.hatena.ne.jp/D_Rascal/20111223/1324646018
*  https://github.com/shiatsumat/wiwinwlh-jp/wiki/%E3%83%A2%E3%83%8A%E3%83%89%E5%A4%89%E6%8F%9B%E5%AD%90

### リポジトリ

```hs
class Monad m => UserRepository m where
  register :: User   -> m (UserId)
  update   :: User   -> m (UserId)
  delete   :: UserId -> m ()
  findBy   :: UserId -> m (Maybe User)
  findAll  :: m ([User])
```

と型クラスで定義することにより抽象レイヤを設けている。  
この使い方はJavaで言う所のインターフェイスと同じと考えて良さそう。

それぞれの関数で型クラス`Monad`に属する型`m`を返している。  
なので、各関数の戻り値の型は`IO`や`Maybe`に変えることができる。  

```hs
instance UserRepository Maybe where
  register u = Nothing
  update u = Nothing
  delete uid = Nothing
  findBy uid = Nothing
  findAll = Nothing
```

これでもOK。

### モック

ここからが本題。  

```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
```

`UserRepository`の実装である`UserMockRepository`を定義。  
さらに型変数`m`を要求していて、`Functor`と`MonadCatch`に属する型であることが必要となる。  

`UserMockRepository`自体の定義を見てみると

```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)
```
用途的に`data`キーワードで定義しても問題なさそうだけど、  
言語拡張`GeneralizedNewtypeDeriving`と組み合わせて記述を簡単にしているっぽい。  

```hs
deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
deriving instance (Functor m, MonadCatch m, MonadIO m) => MonadIO (UserMockRepository m)
```

ここは後追いで`MonadState`と`MonadIO`をderivingしている。  
言語拡張`StandaloneDeriving`を使うと`instance deriving`キーワードを使って、  
後追いでderivingできるようになっているっぽい。

```hs

runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)
runMock (UserMockRepository rp) = runStateT rp
```
`UserMockRepository`と`UserMockState`をパラメータに取り、`m (a, UserMockState)` を返す。  
`m`と`a`は型変数なので、`runStateT :: StateT s m a -> s -> m (a, s)`の戻り値の型`m (a, s)`と  
`runMock`の戻り値`m`と`a`にマッチする。  

上記から`runStateT :: StateT UserMockState m a -> UserMockState -> m (a, UserMockState)`になる。  

```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)
```

最初の型パラメータ`StateT UserMockState m a`は`app`の戻り値と一致する。  
なので、`runStateT`内部では`app`が呼び出されている可能性がある。  

試しに
```hs
app :: StateT UserMockState a m
```
と変えるとコンパイルエラー。  

```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:39:55: error:
• Expected kind ‘* -> *’,
but ‘UserMockRepository m’ has kind ‘(* -> *) -> *’
• In the first argument of ‘UserRepository’, namely
‘UserMockRepository m’
In the instance declaration for
‘UserRepository (UserMockRepository m)’

/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:39:74: error:
• Expecting one more argument to ‘m’
Expected a type, but ‘m’ has kind ‘* -> *’
• In the first argument of ‘UserMockRepository’, namely ‘m’
In the first argument of ‘UserRepository’, namely
‘UserMockRepository m’
In the instance declaration for
‘UserRepository (UserMockRepository m)’
```

試しに`app`を消して`test :: m a`だけにしてもコンパイルエラー。  

```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:34:42: error:
    • Can't make a derived instance of ‘MonadTrans UserMockRepository’
        (even with cunning GeneralizedNewtypeDeriving):
        cannot eta-reduce the representation type enough
    • In the newtype declaration for ‘UserMockRepository’
```

`MonadTrans`で必要な変数らしい。  
ということは`lift`関数で持ち上げされているということ？  

```hs
instance MonadTrans (StateT s) where
    lift m = StateT $ \ s -> do
        a <- m
        return (a, s)
```

`MonatTrans`の実装はこんな感じ。
`GeneralizedNewtypeDeriving`で自動derivingされるっぽい？

`MonadTrans`を今回の型に合わせると`lift` 関数は
```
lift m = StateT $ \ UserMockState -> do
   a <-m
   return (a, UserMockState)
```
になる。

`MonadTrans` / `MonadCatch` / `MonadCatch` をderivingから外してもコンパイルが通った・・・  
`MonadTrans`に注目するのは間違い？

ちょっとアプローチを変えてみる。

#### registerから探ってみる

クラス側の定義はこう。

```
class Monad m => UserRepository m where
  register :: User   -> m (UserId)
```
インスタンス側の定義はこう。
```
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st
```
これがどういう動きをしているか探ってみる。  
戻り値の型は`UserMockRepository m UserId`になる。  

まずは一行目。
```hs
modify $ addUser u
```

`addUser`は自分で定義した関数で  
`addUser :: User -> UserMockState -> UserMockState`となる。

`modify`は`Control.Monad.Trans.State.Lazy`で定義された関数で  
`modify :: Monad m => (s -> s) -> StateT s m ()`となる。

`modify $ addUser u`は`addUser u`が部分適用されて、  
`fn :: UserMockState -> UserMockState`という関数になる。  

`modify`の型変数に当てはめてみる。  
`modify ::  (UserMockState -> UserMockState) -> StateT UserMockState m ()`になる。  

```hs
st <- get
```
続いては`get`。  
`get`は`modify`と同じく、`Control.Monad.Trans.State.Lazy`で定義された関数で  
`get :: Monad m => StateT s m s`となる。  

`st <- get`の変数`st`は`StateT UserMockState m UserMockState`になる。  

```hs
return $ fromJust $ getUserId $ fromJust $ recentry st
```
最後はここ。  

`recentry`は自分で定義した関数（正確にはレコード構文で定義したもの）で  
`recentry :: UserMockState -> Maybe User`となる。  

`st`はこのタイミングでは`UserMockState`となっているようだが、  
この辺は更に調査が必要になりそう。  

`fromJust $ recentry st`で`User`が帰ってくるので、    
`fromJust $ getUserId $ (User型の値)`となるので`UserId`帰る。  

最終的には`return (UserId型の値)`となる。  
`return`により現在のモナドに包むので、  
`UserMockRepository (m UserId) `に包まれる。  

### runMock再考
上記を踏まえてもう一度考えてみる。  
runMockの定義はこう。
```hs
runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)
runMock (UserMockRepository rp) = runStateT rp
```
使われ方はこんな感じ。
```hs
(res, state) <- runMock (SUT.save newUser) $ initState []
```
`save`の中身はこんな感じ。
```hs
save :: IF.UserRepository m => DU.User -> m (DU.UserId)
save u = do
  rs <- findSame u
  case rs of
    Nothing -> IF.register u
    Just x  -> IF.update u
```

`runMock (SUT.save newUser) $ initState []`を  
戻り値の型で示すと`runMock (m (UserId))  UseMockState`となる。  
かつ、型変数`m`は型クラス`UserRepository`に属するものである。  

`runMock`が`UserMockRepository`を要求しているので、  
おそらくこのタイミングで型が`UserMockRepository`と決まる。  
つまり、`UserMockRepository m2 UserId`となる？  
(`m2`は先ほどの型変数`m`とは別なので`m2`として表現している)

試しに`newtype UserMockRepository`を
```hs
newtype UserMockRepository m = UserMockRepository
  {
    app :: StateT UserMockState m UserId
  }
```
に変えるとコンパイルエラー。
```
/Users/gloryof/Development/GitHub/haskell-practice/scotty/scotty-web-api/test/Infra/Repository/UserMock.hs:39:55: error:
    • Expecting one fewer argument to ‘UserMockRepository m’
      Expected kind ‘* -> *’, but ‘UserMockRepository m’ has kind ‘*’
    • In the first argument of ‘UserRepository’, namely
        ‘UserMockRepository m’
      In the instance declaration for
        ‘UserRepository (UserMockRepository m)’
```
39行目というと
```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
```

要求されているカインドが`* -> *`に対して`UserMockRepository m`は`*`というエラー。  
ということは`a -> UserRepository (UserMockRepository m a)`という関数と捉えることができる？   
`Functor`と`MonadCatch`の定義は下記。  
```
class Functor f where

class (MonadThrow e m, Monad n) => MonadCatch e m n | n e -> m where
```
`Functor`のカインドは`*`っぽいので問題なし。  
`MonadCatch`は`UserMockRepository m a`がderivingしているので、  
`n e -> m`は`m a -> UserMockRepository`になっている？  

以上を踏まえて、一個一個解きほぐしていく。
```hs
(res, state) <- runMock (SUT.save newUser) $ initState []
```
まずは自明な型を当てはまめる。  
`(UserID, UserMockState) <- runMock (SUT.save newUser) $ initState []`  
つづいて`runMock`のパラメータの型を当てはめる。  
`(UserId, UserMockState) <- runMock (m UserId) UserMockState`  

これを`runMock`側の定義に当てはめる。
```
runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)
```
戻り値の型とパラメータの型から`a`に`UserId`が当てはまる。  
`runMock :: UserMockRepository m UserId -> UserMockState -> m (UserId, UserMockState)`   
ここだけの定義だと型変数`m`は定まらない。  

`save`ではどうなっているかというと
```
save :: IF.UserRepository m => DU.User -> m (DU.UserId)
save u = do
  rs <- findSame u
  case rs of
    Nothing -> IF.register u
    Just x  -> IF.update u
```
このdoの途中ではまだ`UserMockRepository m UserId`のまま定まっていないはず。  
`register`に到達したタイミングで決まってそう。  

```
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st
```
`modify`は`modify :: Monad m => (s -> s) -> StateT s m ()`という定義。  
`addUser`は`addUser :: User -> UserMockState -> UserMockState`という定義。  
ここも少しずつ解きほぐしていく。

`modify :: Monad m => (s -> s) -> StateT s m ()`
から  
`modify :: (UserMockState -> UserMockState) -> StateT UserMockState m ()`
になる。  
ここのdoにおいては`UserMockRepository (StateT UserMockStaet m ()) UserId`になる？  

`get`は`get :: Monad m => StateT s m s`なので、  
`get :: StateT UserMockState m UserMockState`。  
ここのdoにおいては`UserMockRepository (StateT UserMockState m UserMockState) UserId`になる？  

`return`は`UserMockRepository`がderivingしていて、  
`Monad`のデフォルト実装としては`return :: a -> m a`。  
`UserMockRepository m UserId`に戻る？  

うーん・・・・・・
ある程度辻褄は合ってそうだけど`StateT`のところが上手く説明できていない気がする。  
もっと理解を深める必要がありそう。
