module View.Detail (
  dtlPath,
  view
) where

import qualified Data.Text.Lazy as TL
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS
import qualified Domain.User as DU
import qualified Domain.Age as DA
import qualified Infra.Repository.User as IRU

import qualified View.UserInfo as VUI

dtlPath :: String
dtlPath = "detail.html"

view :: DU.User -> MS.Template -> TL.Text
view u tmp = TL.fromStrict $ MS.substitute tmp $ VUI.convert u
