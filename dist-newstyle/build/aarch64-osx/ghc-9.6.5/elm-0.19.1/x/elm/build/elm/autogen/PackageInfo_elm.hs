{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_elm (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "elm"
version :: Version
version = Version [0,19,1] []

synopsis :: String
synopsis = "The `elm` command line interface."
copyright :: String
copyright = "Copyright (c) 2011-present, Evan Czaplicki"
homepage :: String
homepage = "https://elm-lang.org"
