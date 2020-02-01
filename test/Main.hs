module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import NeatInterpolation
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck


main = defaultMain $ testGroup "" $
  [
    testCase "Demo" $ let
      template a b = 
        [text|
          function(){
            function(){
              $a
            }
            return $b
          }
        |]
      a = "{\n  indented line\n  indented line\n}"
      in assertEqual ""
          "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return {\n    indented line\n    indented line\n  }\n}\n"
          (template a a)
    ,
    testCase "Isolation" $ let
      isolated name = [text|this_could_be_${name}_long_identifier|]
      in assertEqual ""
          "this_could_be_one_long_identifier\n"
          (isolated "one")
    ,
    testCase "Escaping" $ let
      template a b = 
        [text|
          function(){
            function(){
              $a
            }
            return "$$b"
          }
        |]
      escaped name = [text|this_could_be_$$${name}$$_long_identifier|]
      a = "{\n  indented line\n  indented line\n}"
      in do
        assertEqual ""
          "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return \"$b\"\n}\n"
          (template a a)
        assertEqual ""
          "this_could_be_$one$_long_identifier\n"
          (escaped "one")
  ]
