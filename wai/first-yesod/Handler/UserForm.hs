module Handler.UserForm where

import Import

data User = User {
                   name :: Text,
                   age :: Int
                 }

getUserFormR :: Handler Html
getUserFormR = do
  (formData, enctype) <- generateFormPost uForm
  defaultLayout $(widgetFile "user-form")

postUserFormR :: Handler Html
postUserFormR = do
  ((result, formData), enctype) <- runFormPost uForm
  case result of
    FormSuccess _ -> redirect CompleteR
    _ -> defaultLayout $(widgetFile "user-form") 

uForm :: Html -> MForm Handler (FormResult User, Widget)
uForm extra = do
  (nameResult, nameView) <- mreq textField "" Nothing
  (ageResult, ageView) <- mreq intField "" Nothing
  let result = User <$> nameResult <*> ageResult
      widget = $(widgetFile "form-comp")
  return (result, widget);


