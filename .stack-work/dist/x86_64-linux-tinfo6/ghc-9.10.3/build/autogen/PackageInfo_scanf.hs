{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_scanf (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "scanf"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Easy and type-safe format strings for parsing and printing"
copyright :: String
copyright = "2018 Li-yao Xia"
homepage :: String
homepage = "https://github.com/Lysxia/scanf#readme"
