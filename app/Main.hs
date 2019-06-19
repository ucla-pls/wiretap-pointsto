{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

-- base
import           Control.Monad
import           System.Environment
import           System.IO

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
import qualified Data.ByteString.Lazy        as BS

-- wiretap-pointto
import           Wiretap.Data.PointTo
import           Wiretap.Format.PointTo      (getPointToEntries, toPointTo)

main :: IO ()
main = do
  folder:[] <- getArgs

  methods <-
    V.imap Method
    . V.fromList
    . Text.lines
    <$> Text.readFile (folder </> "methods.txt")

  let methodLookup = M.fromList . V.toList . V.map (\b -> (methodAsText b, b)) $ methods

  insts <- V.imap
    (\i a ->
        let
          (n, b) = Text.span (/= '!') a
        in Instruction i
           (methodLookup M.! n)
           (either (error.show) fst . Text.signed Text.decimal $ Text.tail b)
    )
    . V.fromList
    . Text.lines
    <$> Text.readFile (folder </> "instructions.txt")

  let pointstos = folder </> "pointsto"
  es <- listDirectory pointstos >>= mapM
    (\x -> (x,) . runGet getPointToEntries <$> BS.readFile (pointstos </> x)
    )
  v <- fmap S.unions . forM es $ \(name, entries) -> do
    execWriterT $ runStateT (mapM_ (stackMachine . toPointTo insts methods) entries) []


  forM_ v $ \(i, m) ->
    LazyText.putStrLn
    . Builder.toLazyText
    $ methodToText (instMethod i)
    <> Builder.singleton ','
    <> Builder.fromString (show . instOffset $ i)
    <> Builder.singleton ','
    <> methodToText m

stackMachine :: PointTo -> StateT [Instruction] (WriterT (S.Set (Instruction, Method)) IO) ()
stackMachine p = do
  case p of
    BeforeCall i -> do
      modify (i:)
    AfterCall i -> do
      modify (tail . dropWhile (/= i))
    Enter m -> do
      s <- get
      case s of
        r:_ -> tell (S.singleton (r, m))
        _   -> tell (S.empty)

instructionToText :: Instruction -> Builder.Builder
instructionToText ins =
  methodToText (instMethod ins)
  <> Builder.fromText "!"
  <> Builder.fromString (show . instOffset $ ins)

methodToText :: Method -> Builder.Builder
methodToText m =
  Builder.fromText (methodAsText m)
