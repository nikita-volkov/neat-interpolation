module EZInterpolation.Parsing where

import Prelude ()
import ClassyPrelude hiding (try)
import Text.Parsec

data Part =
  PartText [Char] |
  PartIdentifier [Char]

parseParts :: [Char] -> Either ParseError [Part]
parseParts = parse parts "EZInterpolation"
  where
    parts = manyTill (identifier <|> text) eof
    identifier = fmap PartIdentifier $ string "$" *> many1 alphaNum
    text = fmap PartText $ manyTill anyChar $ 
      (try $ void $ lookAhead identifier) <|> eof

