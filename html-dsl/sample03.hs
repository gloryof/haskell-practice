import Html2

headerContent _ =
  [
    meta [] (charset "UTF-8"),
    title "ログインページ"
  ]

bodyContent _ =
  [
    form [] [
              errors(),
              loginInput(),
              button [onclick "hoge()"] (text "ログイン")
            ],
    a [href "/join", cls "join-link", target "_blank"] (text "ユーザ登録はこちら")
  ]

erros _ =
  ul [] [
          li [] [text "メッセージ"]
        ]

loginInput _ =
  dl [] [
          (
            dt [] [text "ログインID"],
            [
              dd [] [ input [tp TEXT, name "username" ] [] ]
            ]
          ),
          (
            dt [] [text "パスワード"],
            [
              dd [] [ input [tp PASSWORD, name "password" ] [] ]
            ]
          )
        ]

libsContent _ =
  [
    js "test.js",
    js "../hoge/test2.js"
  ]

cssContent _ = ["test.css", "../hoge/test2.css" ]

loginPage _ =
  Html {
         doctype = doctypeDefault(),
         header = headerContent,
         body = bodyContent,
         libs = libContent,
         css = cssContent
       }
