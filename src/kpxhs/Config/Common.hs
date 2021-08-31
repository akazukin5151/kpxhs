module Config.Common where

import Brick         (AttrName, attrName)
import Brick.AttrMap (attrNameComponents)

-- FIXME: is there a zero-cost alternative?
-- Basically I want to make an alias to a type constructor
newtype Name = Name [String]
  deriving (Show, Read)

wrapName :: AttrName -> Name
wrapName n = Name $ attrNameComponents n

unwrapName :: Name -> AttrName
unwrapName (Name n) = foldr (\x acc -> attrName x <> acc) mempty n
