{-|
NeatInterpolation provides a quasiquoter for producing strings
with a simple interpolation of input values.
It removes the excessive indentation from the input and
accurately manages the indentation of all lines of interpolated variables.
But enough words, the code shows it better.

Consider the following declaration:

> {-# LANGUAGE QuasiQuotes #-}
>
> import NeatInterpolation
> import Data.Text (Text)
>
> f :: Text -> Text -> Text
> f a b =
>   [trimming|
>     function(){
>       function(){
>         $a
>       }
>       return $b
>     }
>   |]

Executing the following:

> main = Text.putStrLn $ f "1" "2"

will produce this (notice the reduced indentation compared to how it was
declared):

> function(){
>   function(){
>     1
>   }
>   return 2
> }

Now let's test it with multiline string parameters:

> main = Text.putStrLn $ f
>   "{\n  indented line\n  indented line\n}"
>   "{\n  indented line\n  indented line\n}"

We get

> function(){
>   function(){
>     {
>       indented line
>       indented line
>     }
>   }
>   return {
>     indented line
>     indented line
>   }
> }

See how it neatly preserved the indentation levels of lines the
variable placeholders were at?

If you need to separate variable placeholder from the following text to
prevent treating the rest of line as variable name, use escaped variable:

> f name = [trimming|this_could_be_${name}_long_identifier|]

So

> f "one" == "this_could_be_one_long_identifier"

If you want to write something that looks like a variable but should be
inserted as-is, escape it with another @$@:

> f word = [trimming|$$my ${word} $${string}|]

results in

> f "funny" == "$my funny ${string}"
-}
module NeatInterpolation (trimming, untrimming, text) where

import NeatInterpolation.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote hiding (quoteExp)
import qualified Data.Text as Text
import qualified NeatInterpolation.String as String
import qualified NeatInterpolation.Parsing as Parsing


expQQ quoteExp = QuasiQuoter quoteExp notSupported notSupported notSupported where
  notSupported _ = fail "Quotation in this context is not supported"

{-|
An alias to `trimming` for backward-compatibility.
-}
text :: QuasiQuoter
text = trimming

{-|
Trimmed quasiquoter variation.
Same as `untrimming`, but also
removes the leading and trailing whitespace.
-}
trimming :: QuasiQuoter
trimming = expQQ (quoteExp . String.trim . String.unindent . String.tabsToSpaces)

{-|
Untrimmed quasiquoter variation.
Unindents the quoted template and converts tabs to spaces.
-}
untrimming :: QuasiQuoter
untrimming = expQQ (quoteExp . String.unindent . String.tabsToSpaces)

indentQQPlaceholder :: Int -> Text -> Text
indentQQPlaceholder indent text = case Text.lines text of
  head:tail -> Text.intercalate (Text.singleton '\n') $
               head : map (Text.replicate indent (Text.singleton ' ') <>) tail
  [] -> text

quoteExp :: String -> Q Exp
quoteExp input =
  case Parsing.parseLines input of
    Left e -> fail $ show e
    Right lines -> sigE (appE [|Text.intercalate "\n"|] $ listE $ map lineExp lines)
                        [t|Text|]

lineExp :: Parsing.Line -> Q Exp
lineExp (Parsing.Line indent contents) =
  case contents of
    []  -> [| Text.empty |]
    [x] -> toExp x
    xs  -> appE [|Text.concat|] $ listE $ map toExp xs
  where toExp = contentExp (fromIntegral indent)

contentExp :: Integer -> Parsing.LineContent -> Q Exp
contentExp _ (Parsing.LineContentText text) = appE [|Text.pack|] (stringE text)
contentExp indent (Parsing.LineContentIdentifier name) = do
  valueName <- lookupValueName name
  case valueName of
    Just valueName -> do
      appE
        (appE (varE 'indentQQPlaceholder) $ litE $ integerL indent)
        (varE valueName)
    Nothing -> fail $ "Value `" ++ name ++ "` is not in scope"
