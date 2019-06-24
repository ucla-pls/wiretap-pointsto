{-|
Module      : Wiretap.Data.PointTo
Description : Points to information from wiretap
Copyright   : (c) Christian Kalhuage <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module Wiretap.Data.PointTo where

import qualified Data.Text as Text

data Method = Method
  { methodId :: !Int
  , methodAsText :: !Text.Text
  }
  deriving (Show, Eq, Ord)

data Instruction = Instruction
  { instId :: !Int
  , instMethod :: !Method
  , instOffset :: !Int
  } deriving (Show, Eq, Ord)

data PointTo
  = BeforeCall !Instruction
  | AfterCall !Instruction
  | Enter !Method
  deriving (Show, Eq, Ord)

-- data CallEdge =
--   CallEdge !Instruction !
