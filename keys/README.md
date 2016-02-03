型クラスの練習
============

ソースのロード方法
```
Prelude> :l key-sample.hs
```

実行結果(Int)
```
*Main> lock (1::Int)
"Key 1 is locked."
```

実行結果(Char)
```
*Main> lock 'c'
"Key c is locked."
```

実行結果(String)
```
*Main> lock "test"
"Key test is locked."
```

実行結果(Hoge)
```
*Main> lock (Hoge "test1" "test2")
"Key [val1 : test1][val2 : test2] is locked."
```
