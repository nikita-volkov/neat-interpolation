module NeatInterpolation.Prelude 
  ( 
    module Exports,
    (?:),
    traceM,
  )
  where

-- base
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Applicative as Exports
import Control.Arrow as Exports hiding (left, right)
import Control.Category as Exports
import Data.Monoid as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports hiding (for)
import Data.Maybe as Exports
import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Tuple as Exports
import Data.Ord as Exports (Down(..))
import Data.String as Exports
import Data.Int as Exports
import Data.Word as Exports
import Data.Ratio as Exports
import Data.Fixed as Exports
import Data.Ix as Exports
import Data.Data as Exports
import Text.Read as Exports (readMaybe, readEither)
import Control.Exception as Exports hiding (tryJust)
import Control.Concurrent as Exports hiding (yield)
import System.Timeout as Exports
import System.Exit as Exports
import System.IO.Unsafe as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import Unsafe.Coerce as Exports
import GHC.Exts as Exports (groupWith, sortWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Debug.Trace as Exports
import Data.IORef as Exports
import Data.STRef as Exports
import Control.Monad.ST as Exports


(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINE (?:) #-}

traceM :: (Monad m) => String -> m ()
traceM s = trace s $ return ()
