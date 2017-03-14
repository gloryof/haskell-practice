{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Form.UserForm(
  UserForm,
  UserInput,
  empty) where

import Domain.User as DU

import Data.Maybe ()
import qualified Data.Text  as TE()
import qualified Data.Text.Lazy  as TL()
import Data.Validation as VL()

data UserForm = UserForm {
                  errors :: [String],
                  input  :: UserInput
                }

data UserInput = UserInput {
                   iName :: String,
                   iAge  :: String
                 }


cnvFrm :: User -> UserForm
cnvFrm u = UserForm {
                      errors = [],
                      input = UserInput {
                                iName = name u,
                                iAge  = show $ age u
                              }
                    }

empty :: UserForm
empty = UserForm {
                 errors = [],
                 input  = UserInput {
                                      iName   = "",
                                      iAge    = ""
                                    }
               }
