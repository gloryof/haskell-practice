{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Html2 where

data Doctype = Html5 | Html4Strict | Html4Traditional | Html4Frameset

doctypeDefault :: () -> Doctype
doctypeDefault _ = Html5

type Attribute = (String, String)

class Tag x where
  tagName :: x -> String
  emp :: x -> Bool

{- Haders -}
class Tag x => HeadTag x where
  hAtr :: x -> [Attribute]

data MetaTag = MetaTag { metaAttr :: [Attribute]  } deriving Show
meta :: [Attribute] -> MetaTag
meta as = MetaTag { metaAttr = as }

charset :: String -> Attribute
charset x = ("charset", x)

instance Tag MetaTag where
  tagName x = "meta"
  emp x = False

instance HeadTag MetaTag where
  hAtr (MetaTag as) = as

data TitleTag = TitleTag { titleName :: String } deriving Show
title :: String -> TitleTag
title x = TitleTag { titleName = x }

instance Tag TitleTag where
  tagName x = "title"
  emp x = False

instance HeadTag TitleTag where
  hAtr x = []

{- Bodies -}
class Tag x => BodyTag x where
  bAtr :: x -> [Attribute]

class BodyTag x => BlockTag x where
  content :: x -> [BodyTag]


data Html = Html {
              doctype :: Doctype
            }
