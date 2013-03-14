{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Prelude ()
import ClassyPrelude
import EZInterpolation
import EZInterpolation.Parsing
import EZInterpolation.String

main = do
  let a = [text| 
    function startsWith( start, string ){
      return string.lastIndexOf( start ) == 0
    }
  |]
  let pattern = [text| {
      

      $a


      single inlining: ( $a )


      {
        multiple inlining: ( $a ) ( $a )
      }


    } 
    |]
  putStrLn pattern
