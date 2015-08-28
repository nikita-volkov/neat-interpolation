{-# OPTIONS_GHC -F -pgmF htfpp #-}

import BasePrelude
import Test.Framework
import NeatInterpolation


main = htfMain $ htf_thisModulesTests


test_demo = do
  assertEqual
    "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return {\n    indented line\n    indented line\n  }\n}\n"
    (template a a)
  assertEqual
    "this_could_be_one_long_identifier\n"
    (escaped "one")
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
    escaped name = [string|this_could_be_${name}_long_identifier|]
    a = "{\n  indented line\n  indented line\n}"

