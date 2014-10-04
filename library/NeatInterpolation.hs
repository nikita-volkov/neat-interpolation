{-# OPTIONS_GHC -fno-warn-missing-fields #-} 
-- | 
-- NeatInterpolation provides a quasiquoter for producing strings 
-- with a simple interpolation of input values. 
-- It removes the excessive indentation from the input and 
-- accurately manages the indentation of all lines of interpolated variables. 
-- But enough words, the code shows it better.
-- 
-- Consider the following declaration:
-- 
-- > {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
-- > 
-- > import NeatInterpolation
-- > 
-- > f :: String -> String -> String
-- > f a b = 
-- >   [string|
-- >     function(){
-- >       function(){
-- >         $a
-- >       }
-- >       return $b
-- >     }
-- >   |]
-- 
-- Executing the following:
-- 
-- > main = putStrLn $ f "1" "2"
-- 
-- will produce this (notice the reduced indentation compared to how it was
-- declared):
-- 
-- > function(){
-- >   function(){
-- >     1
-- >   }
-- >   return 2
-- > }
-- 
-- Now let's test it with multiline string parameters:
-- 
-- > main = putStrLn $ f 
-- >   "{\n  indented line\n  indented line\n}" 
-- >   "{\n  indented line\n  indented line\n}" 
--
-- We get
--
-- > function(){
-- >   function(){
-- >     {
-- >       indented line
-- >       indented line
-- >     }
-- >   }
-- >   return {
-- >     indented line
-- >     indented line
-- >   }
-- > }
-- 
-- See how it neatly preserved the indentation levels of lines the 
-- variable placeholders were at?  
module NeatInterpolation (string, indentQQPlaceholder) where

import BasePrelude

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import NeatInterpolation.String
import NeatInterpolation.Parsing


-- |
-- The quasiquoter.
string :: QuasiQuoter
string = QuasiQuoter {quoteExp = quoteExprExp}

-- |
-- A function used internally by quasiquoter. Just ignore it.
indentQQPlaceholder :: Int -> String -> String
indentQQPlaceholder indent text = case lines text of
  head:tail -> intercalate "\n" $ head : map (replicate indent ' ' ++) tail
  [] -> text 


quoteExprExp :: String -> Q Exp
quoteExprExp input = 
  case parseLines $ normalizeQQInput input of
    Left e -> fail $ show e
    Right lines -> appE [|unlines|] $ linesExp lines

linesExp :: [Line] -> Q Exp
linesExp [] = [|([] :: [String])|]
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
msumExps = foldr (binaryOpE mappendE) memptyE
memptyE = [|mempty|]
mappendE = [|mappend|]

binaryOpE e = \a b -> e `appE` a `appE` b
