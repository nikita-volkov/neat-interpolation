{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Prelude ()
import ClassyPrelude
import EZInterpolation

main = do
  let a = [text| 
    function startsWith( start, string ){
      return string.lastIndexOf( start ) == 0
    }
  |]
  putStrLn $ [text| {
      $a
      single inlining: ( $a )
      {
        multiple inlining: ( $a ) ( $a )
      }
    } 

    |]
