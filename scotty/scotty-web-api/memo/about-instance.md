### instance
```hs
newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
```
この型に対して

```
instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
```
という定義は部分適用で実際は型変数`a`を要求している？
