{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} 
module EZInterpolation (text) where

import Prelude ()
import ClassyPrelude

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import EZInterpolation.String
import EZInterpolation.Parsing


text :: QuasiQuoter
text = QuasiQuoter {quoteExp = quoteExprExp}

quoteExprExp :: [Char] -> TH.ExpQ
quoteExprExp = TH.stringE . normalizeQQInput
