{-# OPTIONS_GHC -F -pgmF htfpp #-}

import BasePrelude
import Test.Framework
import NeatInterpolation


main = htfMain $ htf_thisModulesTests


test_demo =
  assertEqual
    "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return {\n    indented line\n    indented line\n  }\n}\n"
    (template a a)
  where
    template a b = 
      [string|
        function(){
          function(){
            $a
          }
          return $b
        }
      |]
    a = "{\n  indented line\n  indented line\n}"

