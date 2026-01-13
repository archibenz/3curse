{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_cuthill_mckee_hs (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "cuthill_mckee_hs"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Cuthill-McKee ordering algorithms for matrix structure"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
