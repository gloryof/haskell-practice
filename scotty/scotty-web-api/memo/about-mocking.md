## モックについて考察

いまいち理解できていないのでメモ。  
参考にさせていただいたサイトは下記。  
* http://krdlab.hatenablog.com/entry/2015/11/03/122732
* http://straitwalk.hatenablog.com/entry/20120917/1347862238
* http://d.hatena.ne.jp/D_Rascal/20111223/1324646018

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
Javaで言う所のインターフェイスと同じと考えて良さそう。

それぞれの関数で型クラス`Monad`に属する型`m`を返している。  
なので、各関数の戻り値の型は`IO`や`Maybe`に変えることができる。  

```hs
Instance UserRepository Maybe where
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
