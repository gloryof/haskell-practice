## Stateモナド

### よくわからない箇所

```hs
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st
```

最後の行で`UserMockRepository m UserId`になっているんだろうなぁとは思うものの  
本当にそうなのか？途中の行の動きはどうなっているのか？を把握できていない。  

`UserMockRepository`の定義はこう。
```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)

deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
deriving instance (Functor m, MonadCatch m, MonadIO m) => MonadIO (UserMockRepository m)
```

どこまでが動作がする最低限のものかを調べてみたところこんな感じ。
```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad)

deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
```

#### modify

`register`関数の処理に戻る。  
最初の行で`modify $ addUser u`は  
`modify :: (UserMockState -> UserMockState) -> StateT UserMockState m ()`になる。  

ふと気づいたけど `UserMockRepository $ (modify $ addUser u)` これがコンパイルを通る。  
do記法をとくとこんな感じで通る。
```hs
register u = do
  (UserMockRepository $ (modify $ addUser u))
  >>= (\_ -> get)
  >>= (\st -> return $ fromJust $ getUserId $ fromJust $ recentry st )
```
つまり`modify`の行は`modify`を実行し`UserMockRepository`に変換しているだけ。

#### get

次、`st <- get`の行。  
`get :: StateT UserMockState m UserMockState`になる。  
`get`も`modify`同じく`UserMockRepository`に変換できる。

#### 最後の行

値変数`st`は`UserMockState`なので、
産後の行では`recentry` -> `getUserId`と呼ばれて`return $ UserId`となる。  
`UserMockRepository`は`UserMockRepository m a`で`Monad`をderivingしているので、
おそらく`return :: a -> UserMockRepository m a`のはず。  

#### それでもわからないこと

- `modify`も`get`も`return`を介せず`UserMockRepository`に変換できているのはなぜか？
- `UserMockRepository`をreturnした時のステートはどこから取ってきているのか？


どちらも`MonadState`をderivingしていることが要因っぽい？

### MonadState

実際は`MonadState`による動作だったので詳しく調べて行く。

調べるにあたって下記の定義が若干厄介？
```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)

deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
```

`newtype`で定義してる型は`StateT userMockState m a`なので、  
`instance MonadState s UserMockRepository`ではなく、
`instance Monad m => MonadState s (StateT s m)`をみるべきっぽい。  

二つある。
```hs
instance Monad m => MonadState s (Lazy.StateT s m) where
    get = Lazy.get
    put = Lazy.put
    state = Lazy.state

instance Monad m => MonadState s (Strict.StateT s m) where
    get = Strict.get
    put = Strict.put
    state = Strict.state
```
どちらだろうか？  
基本は遅延評価だからLazyの方を調べてみる。

StateTの定義。  
```hs
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
```

#### modify

`modify :: MonadState s m => (s -> s) -> m ()`という定義。  
`(UserMockState -> UserMockState) -> UserMockRepository ()`となる？
ただ、`UserMockRepository m a`という定義なので、 `UserMockRepository m ()`になる方がしっくりくる気がする。  

コードはこんな感じ。
```hs
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)
```

`state`のコードはこう。
```hs
state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)
```
`modify`で渡された関数`f`を適用して、  
適用した戻り値の型（アクション？）`()`と変更されたステートを返す。

#### get
`get :: m s`という定義。  
`UserMockRepository UserMockState`になる？  
ただ、 `UserMockRepository m UserMockState`になる方がしっくりくる気がする。


コードはこんな感じ。
```hs
get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)
```

今のステートを関数の戻り値、新しいステートとして返す。  

#### 既存のステートはどこからきているのか？

データの初期設定はこんな感じ。
```hs
runMock (SUT.save newUser) $ initState []
```

`runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)`と
`initState :: [User] -> UserMockState`と
`save :: IF.UserRepository m => DU.User -> m (DU.UserId)`。

`save newUser`は最終的に`register newUser`とほぼ同意義。

実行されている部分のみ抜粋。
```hs
runMock (register newUser) $ initState []

register u = do
  modify $ addUser u
  st <- get
  return $ fromJust $ getUserId $ fromJust $ recentry st
```

`runMock (register newUser) $ initState []`で
`UserMockRepository (StateT UserMockState{value = []} m a) `が構築される？  

んで、`StateT UserMockState{value = []} m a`という値に対して、  
`modify $ addUser u`が適用されて、  
`StateT UserMockState{value = [値変数u]} m ()`になる。

`StateT UserMockState{value = [値変数u]} m a`という値に対して、
`get`が適用されて、  
`StateT UserMockState{value = [値変数u]} m UserMockState{value = [値変数u]}`になる。  

`StateT UserMockState{value = [値変数u]} m UserMockState{value = [値変数u]}`に対して、  
最後の行が評価されて、  
`StateT UserMockState{value = [値変数u]} m UserId`になる。  

`return`で`UserMockRepository m UserId`になる。かな？  

っで、この辺は遅延評価なので順番通りにはいかない。  

その辺は遅延評価で調べる。
