module View.Render (
  Renderer,
  path,
  val
  ) where

import Text.Mustache.Types (Value)

class Renderer x where
  path :: x -> String
  val  :: x -> Value
