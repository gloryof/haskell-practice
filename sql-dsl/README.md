SQLのDSLっぽいものを作る
==========

# 構文

`
rel "hoge" => "SELECT * FROM hoge"
`

`
rel "hoge" . extract ["fuga = 1"] => "SELECT * FROM hoge WHERE fuga = 1"
`

`
rel "hoge" . extract ["fuga = 1"] . proj ["fuga, bar"] => "SELECT fuga, bar FROM hoge WHERE fuga = 1"
`