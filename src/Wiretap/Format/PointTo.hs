{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-|
Module      : Wiretap.Format.PointTo
Description : Points to information from wiretap
Copyright   : (c) Christian Kalhuage <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module Wiretap.Format.PointTo where

-- binary
import           Data.Binary
import           Data.Binary.Get

-- base
import           Control.Monad
import           Data.Int
import           GHC.Generics         (Generic)

-- vector
import qualified Data.Vector          as V

-- wiretap-pointto
import qualified Wiretap.Data.PointTo as D

data PointToEntry
  = BeforeCall {-# UNPACK #-} !Int32
  | AfterCall {-# UNPACK #-} !Int32
  | Enter {-# UNPACK #-} !Int32
  deriving (Show, Eq, Generic)

instance Binary PointToEntry

getPointToEntries :: Get [PointToEntry]
getPointToEntries =
  isEmpty >>= \case
  True ->  return []
  False -> liftM2 (:) get getPointToEntries

toPointTo :: V.Vector D.Instruction -> V.Vector D.Method -> PointToEntry -> D.PointTo
toPointTo inst meths = \case
  BeforeCall i -> D.BeforeCall (inst V.! fromIntegral i)
  AfterCall i  -> D.AfterCall  (inst V.! fromIntegral i)
  Enter m -> D.Enter (meths V.! fromIntegral m)
