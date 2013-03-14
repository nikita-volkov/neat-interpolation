{-# LANGUAGE NoMonomorphismRestriction #-}
module EZInterpolation.Parsing where

import Prelude ()
import ClassyPrelude hiding (try, lines)
import Text.Parsec hiding (Line)

data Line = 
  Line {lineIndent :: Int, lineContents :: [LineContent]}
  deriving (Show)

data LineContent = 
  LineContentText [Char] |
  LineContentIdentifier [Char]
  deriving (Show)


parseLines :: [Char] -> Either ParseError [Line]
parseLines = parse lines "EZInterpolation.Parsing.parseLines"
  where
    lines = sepBy line newline <* eof
    line = Line <$> countIndent <*> many content
    countIndent = fmap length $ try $ lookAhead $ many $ char ' '
    content = try identifier <|> contentText
    identifier = fmap LineContentIdentifier $ string "$" *> many1 alphaNum
    contentText = do
      text <- manyTill anyChar end
      if null text
        then fail "Empty text"
        else return $ LineContentText $ text
      where
        end = 
          (void $ try $ lookAhead identifier) <|> 
          (void $ try $ lookAhead newline) <|> 
          eof
