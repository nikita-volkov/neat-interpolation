module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import NeatInterpolation
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck

main = defaultMain $ testGroup "" $
  [
    testCase "Demo" $ let
      template a b = 
        [trimming|
          function(){
            function(){
              $a
            }
            return $b
          }
        |]
      a = "{\n  indented line\n  indented line\n}"
      in assertEqual ""
          "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return {\n    indented line\n    indented line\n  }\n}"
          (template a a)
    ,
    testCase "Isolation" $ let
      isolated name = [trimming|this_could_be_${name}_long_identifier|]
      in assertEqual ""
          "this_could_be_one_long_identifier"
          (isolated "one")
    ,
    testCase "Escaping 1" $ let
      template a b = 
        [trimming|
          function(){
            function(){
              $a
            }
            return "$$b"
          }
        |]
      a = "{\n  indented line\n  indented line\n}"
      in assertEqual ""
          "function(){\n  function(){\n    {\n      indented line\n      indented line\n    }\n  }\n  return \"$b\"\n}"
          (template a a)
    ,
    testCase "Escaping 2" $ let
      escaped name = [trimming|this_could_be_$$${name}$$_long_identifier|]
      in assertEqual ""
          "this_could_be_$one$_long_identifier"
          (escaped "one")
    ,
    testCase "Deindentation" $ let
      template fieldName className = [trimming|
          * @param $fieldName value of the {@code $fieldName} property of
                   the {@code $className} case
        |]
      in assertEqual ""
          "* @param a value of the {@code a} property of\n         the {@code b} case"
          (template "a" "b")
    ,
    testGroup "Polymorphism"
      [ testCase "String"          $ assertEqual "" [trimming|foo|] ("foo" :: String)
      , testCase "Text"            $ assertEqual "" [trimming|foo|] ("foo" :: T.Text)
      , testCase "Lazy Text"       $ assertEqual "" [trimming|foo|] ("foo" :: TL.Text)
      , testCase "ByteString"      $ assertEqual "" [trimming|foo|] ("foo" :: BS.ByteString)
      , testCase "Lazy ByteString" $ assertEqual "" [trimming|foo|] ("foo" :: BL.ByteString)
      ]
  ]
