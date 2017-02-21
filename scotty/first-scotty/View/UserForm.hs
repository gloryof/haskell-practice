{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module View.UserForm(view, rview, confirm) where

import Domain.User as DU

import Data.Maybe ()
import Data.Aeson
import Data.Text
import Data.Text.Lazy as TL
import Data.Validation as VL

import Web.Scotty as WS
import Text.EDE
import Text.Blaze.Html.Renderer.Text (renderHtml)

data UserForm = UserForm {
                  errors :: [String],
                  input  :: UserInput
                }

instance ToJSON UserForm

data UserInput = UserInput {
                   iName :: String,
                   iAge  :: String
                 }

view :: ActionM ()
view = (renderIn $ emp) >>= html 

rview :: ActionM ()
rview = do
          inp  <- ext
          form <- return $ UserForm { errors = [], input = inp }
          renIo <- return $ renderIn $ form
          do
            ren <- renIo
            case ren of
              Left  _ -> html $ TL.pack "error"
              Right t -> html t

confirm :: ActionM ()
confirm = do
            inp <- ext
            rst <- return $ DU.parseUser (iName inp) (iAge inp)
            case rst of
              VL.Failure es -> do
                                 rin <- return $ renderIn $ UserForm { errors = es, input = inp }
                                 case rin of
                                   Left  _ -> raise $ TL.pack "error"
                                   Right t -> html t
              VL.Success u  -> do
                                 rcn <- return $  renderCn u
                                 case rcn of
                                   Left  _ -> raise $ TL.pack "error"
                                   Right t -> html t

ext :: ActionM UserInput
ext = do
        nm <- param "name"
        ag <- param "age"
        return UserInput { iName = nm, iAge = ag }


renderIn :: UserForm -> IO (Either String TL.Text)
renderIn uf = do
                parsed <- (eitherParseFile "template/form-input.html")
                parsed >>= (`eitherRender` it)
              where
                it = fromPairs [ "it" .= toJSON uf ]

renderCn :: User -> Either String TL.Text
renderCn u = do
               parsed <- eitherParseFile "template/form-confirm.html"
               parsed >>= (`eitherRender` it)
             where it   = fromPairs [ "it" .= toJSON  $ cnvFrm u ]

cnvFrm :: User -> UserForm
cnvFrm u = UserForm {
                      errors = [],
                      input = UserInput {
                                iName = name u,
                                iAge  = show $ age u
                              }
                    }

emp :: UserForm
emp = UserForm {
                 errors = [],
                 input  = UserInput {
                                      iName   = "",
                                      iAge    = ""
                                    }
               }
