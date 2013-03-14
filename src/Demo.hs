{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Prelude ()
import ClassyPrelude
import NeatInterpolation
import NeatInterpolation.Parsing
import NeatInterpolation.String

main = do
  let a' = [text| 
    function startsWith( start, string ){
      return string.lastIndexOf( start ) == 0
    }
  |]
  let pattern = [text| {
      

      $a'

      single inlining: ( $a' )


      {
        multiple inlining: ( $a' ) ( $a' )
      }


    } 
    |]
  putStrLn pattern
