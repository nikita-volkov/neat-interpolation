{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} 
module NeatInterpolation (text, indentQQPlaceholder) where

import Prelude ()
import ClassyPrelude

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import NeatInterpolation.String
import NeatInterpolation.Parsing



text :: QuasiQuoter
text = QuasiQuoter {quoteExp = quoteExprExp}

indentQQPlaceholder :: Int -> Text -> Text
indentQQPlaceholder indent text = case lines text of
  head:tail -> intercalate "\n" $ head : map (replicate indent " " ++) tail
  [] -> text 


quoteExprExp :: [Char] -> Q Exp
quoteExprExp input = 
  case parseLines $ normalizeQQInput input of
    Left e -> fail $ show e
    Right lines -> appE [|unlines|] $ linesExp lines

linesExp :: [Line] -> Q Exp
linesExp [] = [|([] :: [Text])|]
linesExp (head : tail) = 
  (binaryOpE [|(:)|])
    (lineExp head)
    (linesExp tail)

lineExp :: Line -> Q Exp
lineExp (Line indent contents) = 
  msumExps $ map (contentExp $ fromIntegral indent) contents



contentExp :: Integer -> LineContent -> Q Exp
contentExp _ (LineContentText text) = stringE text
contentExp indent (LineContentIdentifier name) = do
  valueName <- lookupValueName name
  case valueName of
    Just valueName -> do
      Just indentQQPlaceholderName <- lookupValueName "indentQQPlaceholder"
      appE
        (appE (varE indentQQPlaceholderName) $ litE $ integerL indent)
        (varE valueName)
    Nothing -> fail $ "Value `" ++ name ++ "` is not in scope"

msumExps :: [Q Exp] -> Q Exp
msumExps = fold (binaryOpE mappendE) memptyE
memptyE = [|mempty|]
mappendE = [|mappend|]

binaryOpE e = \a b -> e `appE` a `appE` b
