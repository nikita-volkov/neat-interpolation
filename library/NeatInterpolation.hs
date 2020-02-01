-- |
-- NeatInterpolation provides a quasiquoter for producing strings
-- with a simple interpolation of input values.
-- It removes the excessive indentation from the input and
-- accurately manages the indentation of all lines of interpolated variables.
-- But enough words, the code shows it better.
--
-- Consider the following declaration:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > import NeatInterpolation
-- > import Data.Text (Text)
-- >
-- > f :: Text -> Text -> Text
-- > f a b =
-- >   [text|
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
-- > main = T.putStrLn $ f "1" "2"
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
-- > main = T.putStrLn $ f
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
--
-- If you need to separate variable placeholder from the following text to
-- prevent treating the rest of line as variable name, use escaped variable:
--
-- > f name = [text|this_could_be_${name}_long_identifier|]
--
-- So
--
-- > f "one" == "this_could_be_one_long_identifier"
--
-- If you want to write something that looks like a variable but should be
-- inserted as-is, escape it with another @$@:
--
-- > f word = [text|$$my ${word} $${string}|]
--
-- results in
--
-- > f "funny" == "$my funny ${string}|]
module NeatInterpolation (text) where

import NeatInterpolation.Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Quote hiding (quoteExp)

import NeatInterpolation.String
import NeatInterpolation.Parsing

import Data.Text (Text)
import qualified Data.Text as T


-- |
-- The quasiquoter.
text :: QuasiQuoter
text = QuasiQuoter quoteExp notSupported notSupported notSupported where
  notSupported _ = fail "Quotation in this context is not supported"

indentQQPlaceholder :: Int -> Text -> Text
indentQQPlaceholder indent text = case T.lines text of
  head:tail -> T.intercalate (T.singleton '\n') $
               head : map (T.replicate indent (T.singleton ' ') <>) tail
  [] -> text

quoteExp :: String -> Q Exp
quoteExp input =
  case parseLines $ normalizeQQInput input of
    Left e -> fail $ show e
    Right lines -> sigE (appE [|T.intercalate "\n"|] $ listE $ map lineExp lines)
                        [t|Text|]

lineExp :: Line -> Q Exp
lineExp (Line indent contents) =
  case contents of
    []  -> [| T.empty |]
    [x] -> toExp x
    xs  -> appE [|T.concat|] $ listE $ map toExp xs
  where toExp = contentExp (fromIntegral indent)

contentExp :: Integer -> LineContent -> Q Exp
contentExp _ (LineContentText text) = appE [|T.pack|] (stringE text)
contentExp indent (LineContentIdentifier name) = do
  valueName <- lookupValueName name
  case valueName of
    Just valueName -> do
      appE
        (appE (varE 'indentQQPlaceholder) $ litE $ integerL indent)
        (varE valueName)
    Nothing -> fail $ "Value `" ++ name ++ "` is not in scope"
