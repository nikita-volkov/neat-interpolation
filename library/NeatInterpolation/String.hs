module NeatInterpolation.String where

import NeatInterpolation.Prelude


unindent :: [Char] -> [Char]
unindent s =
  case lines s of
    head : tail -> 
      let 
        unindentedHead = dropWhile (== ' ') head 
        minimumTailIndent = minimumIndent . unlines $ tail
        unindentedTail = case minimumTailIndent of
          Just indent -> map (drop indent) tail
          Nothing -> tail
      in unlines $ unindentedHead : unindentedTail
    [] -> []

trim :: [Char] -> [Char]
trim = dropWhileRev isSpace . dropWhile isSpace

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

tabsToSpaces :: [Char] -> [Char]
tabsToSpaces ('\t' : tail) = "    " ++ tabsToSpaces tail
tabsToSpaces (head : tail) = head : tabsToSpaces tail
tabsToSpaces [] = []

minimumIndent :: [Char] -> Maybe Int
minimumIndent = 
  listToMaybe . sort . map lineIndent 
    . filter (not . null . dropWhile isSpace) . lines

-- | Amount of preceding spaces on first line
lineIndent :: [Char] -> Int
lineIndent = length . takeWhile (== ' ')
