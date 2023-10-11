module Main where

import NeatInterpolation
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (choose)

main :: IO ()
main =
  defaultMain $
    testGroup "" $
      [ testCase "Demo" $
          let template a b =
                [trimming|
          function(){
            function(){
              $a
            }
            return $b
          }
        |]
              a = "{\n  indented line\n  indented line\n}"
           in assertEqual
                ""
                "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return {\n    indented line\n    indented line\n  }\n}"
                (template a a),
        testCase "Isolation" $
          let isolated name = [trimming|this_could_be_${name}_long_identifier|]
           in assertEqual
                ""
                "this_could_be_one_long_identifier"
                (isolated "one"),
        testCase "Escaping 1" $
          let template a b =
                [trimming|
          function(){
            function(){
              $a
            }
            return "$$b"
          }
        |]
              a = "{\n  indented line\n  indented line\n}"
           in assertEqual
                ""
                "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return \"$b\"\n}"
                (template a a),
        testCase "Escaping 2" $
          let escaped name = [trimming|this_could_be_$$${name}$$_long_identifier|]
           in assertEqual
                ""
                "this_could_be_$one$_long_identifier"
                (escaped "one"),
        testCase "Deindentation" $
          let template fieldName className =
                [trimming|
          * @param $fieldName value of the {@code $fieldName} property of
                   the {@code $className} case
        |]
           in assertEqual
                ""
                "* @param a value of the {@code a} property of\n         the {@code b} case"
                (template "a" "b")
      ]
