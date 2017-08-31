## 遅延評価

### 参考にさせていただいたサイト

http://itchyny.hatenablog.com/entry/20130209/1360417348
http://d.hatena.ne.jp/kazu-yamamoto/20151117/1447726679

### 書いたコードの動き

標準出力に文字を出力してどういう順番で実行されるかを試してみる。  
試してみたコードは下記に抜粋する。
```hs
initState :: [User] -> UserMockState
initState us = trace (show "initState") $ UserMockState
  {
   recentry  = case us of
                  [] -> Nothing
                  _  -> Just $ last us,
    users     = us
  }


runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)
runMock (UserMockRepository rp) = trace (show "runMock") runStateT rp

register u = do
  modify $ trace (show "modify row") $ addUser u
  st <- trace (show "get row") get
  return $ trace (show "return row") $ fromJust $ getUserId $ fromJust $ recentry st

(res, state) <- runMock (SUT.save newUser) $ initState []
```

予想では
```
"runMock"
"initState"
"modify row"
"get row"
"return row"
```
だったんだけど実際はこう。
```
"runMock"
"get row"
"return row"
"modify row"
"initState"
```

### 正格評価

正格評価にする言語拡張があるので拡張してみる。
```hs
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
```

改めて実行してみると。
```
"runMock"
"get row"
"modify row"
"initState"
"return row"
```
"initState"以外は予想通りの順番になった。  
なぜ"initState"だけ違うのかというのは気になる。

### 試してみる

```hs
test :: Int
test =
  x + y
  where
    x = y + 1
    y = x + 2
```

コンパイルは通るけど、実行すると・・・
```
  test/UseCase/UserSpec.hs:32:
  1) UseCase.User.save New user add to repository. Return generated UserId
       uncaught exception: NonTermination (<<loop>>)

  test/UseCase/UserSpec.hs:46:
  2) UseCase.User.save Exisist user is update.
       uncaught exception: NonTermination (<<loop>>)
```
エラー。


### まとめ

今の段階では下記を理解していれば問題なさそう。

- 型の定義があっていればコンパイルは通る
- 評価時に値が決まっていればいいので、実行順番は気にしなくていい
