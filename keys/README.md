型クラスの練習
============

ソースのロード方法
```
:l key-sample.hs
```

実行結果(Int)
```
lock (1::Int)
"Key 1 is locked."
```

実行結果(Char)
```
lock 'c'
"Key 'c' is locked."
```

実行結果(Hoge)
```
lock (Hoge "test1" "test2")
"Key [val1 : test1][val2 : test2] is locked."
```
