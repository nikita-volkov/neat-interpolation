module NeatInterpolation.Parsing where

import NeatInterpolation.Prelude hiding (many, some, try, (<|>))
import Data.Text (Text, pack)
import Text.Megaparsec hiding (Line)
import Text.Megaparsec.Char

data Line =
  Line {lineIndent :: Int, lineContents :: [LineContent]}
  deriving (Show)

data LineContent =
  LineContentText [Char] |
  LineContentIdentifier [Char]
  deriving (Show)

type Parser = Parsec Void String

-- | Pretty parse exception for parsing lines.
newtype ParseException = ParseException Text
    deriving (Show, Eq)

parseLines :: [Char] -> Either ParseException [Line]
parseLines input = case parse lines "NeatInterpolation.Parsing.parseLines" input of
    Left err -> Left $ ParseException $ pack $ errorBundlePretty err
    Right output -> Right output
  where
    lines :: Parser [Line]
    lines = sepBy line newline <* eof
    line = Line <$> countIndent <*> many content
    countIndent = fmap length $ try $ lookAhead $ many $ char ' '
    content = try escapedDollar <|> try identifier <|> contentText
    identifier = fmap LineContentIdentifier $
      char '$' *> (try identifier' <|> between (char '{') (char '}') identifier')
    escapedDollar = fmap LineContentText $ char '$' *> count 1 (char '$')
    identifier' = some (alphaNumChar <|> char '\'' <|> char '_')
    contentText = do
      text <- manyTill anySingle end
      if null text
        then fail "Empty text"
        else return $ LineContentText $ text
      where
        end =
          (void $ try $ lookAhead escapedDollar) <|>
          (void $ try $ lookAhead identifier) <|>
          (void $ try $ lookAhead newline) <|>
          eof
