import Html
import HtmlPres

messages _ = ul [(sCls ["error-message"])]
                [
                  (li [] [text("メッセージ")]),
                  (li [] [text("メッセージ2")]),
                  (li [] [text("メッセージ3")])
                ]

inputs _ = dl []
              [
                (dt [] [ text("ログイン")] ),
                (dd [] [ (input [] TEXT "username") ]),
                (dt [] [ text("パスワード")] ),
                (dd [] [ (input [] PASSWORD "username")])
              ]

loginForm _ = form [
                    (sid "test-form"),
                    (method POST),
                    (sCls ["form-class"])
                   ]
                   [
                     messages(),
                     inputs(),
                     button [] "ログイン"
                   ]

loginPage _ = Html {
  hd = [
         (meta "charset" "UTF-8"),
         (title "ログインページ")
       ],
  body = [
           loginForm(),
           (a [("href", "/join")] "ユーザ登録はこちら")
         ]
}
