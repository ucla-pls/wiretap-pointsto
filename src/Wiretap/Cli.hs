{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase     #-}
{-|
Module      : Wiretap.Cli
Description : Command line interface to work with wiretap
Copyright   : (c) Christian Kalhuage <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}

module Wiretap.Cli where

-- base
import           Control.Monad
import           Data.Maybe
import           Data.IORef
import           System.Environment
import           System.IO
import qualified Data.List.NonEmpty as NE

-- binary
import           Data.Binary.Get

-- vector
import qualified Data.Vector                 as V

-- filepath
import           System.FilePath

-- directory
import           System.Directory

-- containers
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S

-- text
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Text.Lazy.IO           as LazyText
import qualified Data.Text.Read              as Text

-- mtl
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

-- bytestring
import qualified Data.ByteString.Lazy        as BL

-- wiretap-pointto
import           Wiretap.Data.PointTo
import           Wiretap.Format.PointTo      (streamPointToEntries, toPointTo, PointToEntry)

main :: IO ()
main = do
  folder:[] <- getArgs

  methods <- readMethods folder
  insts <- readInstructions folder methods

  let pointstos = folder </> "pointsto"

  u <- newIORef (S.empty)
  files <- listDirectory pointstos
  forM_ files (\x -> processFile insts methods u (pointstos </> x))
  where
    readMethods folder =
      V.imap Method
      . V.fromList
      . Text.lines
      <$> Text.readFile (folder </> "methods.txt")

    readInstructions folder methods = do
      instructions <-
        V.fromList
        . Text.lines
        <$> Text.readFile (folder </> "instructions.txt")

      return . flip V.imap instructions $ \i a ->
        let (n, b) = Text.span (/= '!') a
        in Instruction i
          (methodLookup M.! n)
          (either (error.show) fst . Text.signed Text.decimal $ Text.tail b)

      where
        methodLookup =
          M.fromList . V.toList . V.map (\b -> (methodAsText b, b)) $ methods


type Uniq a = IORef (S.Set a)

checkUnique u a = do
  s <- readIORef u
  let isUnqiue = a `S.notMember` s
  when isUnqiue $ writeIORef u (a `S.insert` s)
  return isUnqiue

processFile insts methods u file = do
  stack <- newIORef endOfStack
  hPutStrLn stderr ("Reading " ++ file)
  entries <- BL.readFile file
  streamPointToEntries insts methods (processEntry u stack) (error . ("Error" ++)) entries

processEntry ::
  Uniq (Instruction, Method)
  -> IORef (NE.NonEmpty Instruction)
  -> PointTo
  -> IO ()
processEntry u s = \case
  BeforeCall i -> do
    modifyIORef' s (i NE.<|)
  AfterCall i -> do
    modifyIORef' s (popUntil i)
  Enter m -> do
    r <- NE.head <$> readIORef s
    b <- checkUnique u (r, m)
    when b $ writeEdge r m

  where
    popUntil a as = fromMaybe endOfStack $ do
      as' <- NE.nonEmpty $ NE.dropWhile (/= a) as
      NE.nonEmpty . NE.tail $ as'
{-# INLINE processEntry #-}

endOfStack = Instruction (-1) (Method (-1) "<boot>") 0 NE.:| []

writeEdge i m =
  LazyText.putStrLn
  . Builder.toLazyText
  $ methodToText (instMethod i)
  <> Builder.singleton ','
  <> Builder.fromString (show . instOffset $ i)
  <> Builder.singleton ','
  <> methodToText m

  where
    methodToText :: Method -> Builder.Builder
    methodToText m =
      Builder.fromText (methodAsText m)
