module Main where

import Prelude

import Brainfuck.Binaryen (WasmCellSize, cell0)
import Brainfuck.Compiler (compile)
import Brainfuck.IR (showIR, viewIR)
import Brainfuck.Optimizer (optimize)
import Brainfuck.Parser (parse)
import Brainfuck.Runtime (runBinary)
import Brainfuck.Transpiler (transpile)
import Data.Array (index)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, codePointAt, codePointFromChar, fromCodePointArray)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Ref (modify_, new, read)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (argv, stdout)
import Node.Stream (writeString)
import Unsafe.Coerce (unsafeCoerce)

config
  :: { cellSize :: WasmCellSize
     , importModule :: String
     , inputFunction :: String
     , mainFunction :: String
     , outputFunction :: String
     , cellCount :: Int
     }
config =
  { importModule: "env"
  , inputFunction: "input"
  , outputFunction: "output"
  , mainFunction: "main"
  , cellSize: cell0
  , cellCount: 10000
  }

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ argv
  let fileMaybe = index args 2
  let input = fromMaybe "" $ index args 3
  case fileMaybe of
    Nothing -> log "No file provided"
    Just file -> do
      raw <- readTextFile UTF8 file
      log $ "Loaded " <> file

      log $ "Parsing..."
      Milliseconds beforeParse <- unInstant <$> liftEffect now
      let
        ast = parse raw
      Milliseconds afterParse <- unInstant <$> liftEffect now
      log "Parsed"

      log $ "Transpiling..."
      Milliseconds beforeTranspile <- unInstant <$> liftEffect now
      let
        ir = transpile ast
      Milliseconds afterTranspile <- unInstant <$> liftEffect now
      log "Transpiled"

      log $ "Optimizing..."
      Milliseconds beforeOptimize <- unInstant <$> liftEffect now
      let
        ir' = optimize 3000 ir
      Milliseconds afterOptimize <- unInstant <$> liftEffect now
      log "Optimizing"

      log "Compiling..."
      Milliseconds beforeCompile <- unInstant <$> liftEffect now
      binary <- liftEffect $
        compile
          { cellSize: config.cellSize, importModule: config.importModule, inputFunction: config.inputFunction, outputFunction: config.outputFunction, mainFunction: config.mainFunction }
          ir'
      Milliseconds afterCompile <- unInstant <$> liftEffect now
      log "Compiled"

      inputIndex <- liftEffect $ new 0
      let
        inputFunc :: Effect CodePoint
        inputFunc = do
          idx <- liftEffect $ read inputIndex
          case codePointAt idx input of
            Just c -> do
              liftEffect $ modify_ (_ + 1) inputIndex
              pure c
            Nothing -> pure $ unsafeCoerce 0

        outputFunc :: CodePoint -> Effect Unit
        outputFunc c = void $ writeString stdout UTF8 $ fromCodePointArray [ c ]

      log "Running..."
      log "== OUTPUT =="
      Milliseconds beforeRun <- unInstant <$> liftEffect now
      liftEffect $ runBinary
        { importModule: config.importModule, inputFunction: config.inputFunction, outputFunction: config.outputFunction, mainFunction: config.mainFunction }
        inputFunc
        outputFunc
        binary
      Milliseconds afterRun <- unInstant <$> liftEffect now
      log ""
      log "============"

      log $ "Parse time: " <> show (afterParse - beforeParse)
      log $ "Transpile time: " <> show (afterTranspile - beforeTranspile)
      log $ "Optimize time: " <> show (afterOptimize - beforeOptimize)
      log $ "Compile time: " <> show (afterCompile - beforeCompile)
      log $ "Run time: " <> show (afterRun - beforeRun)
      log ""
      log $ "Total time: " <> show (afterRun - beforeParse)
      log "---------"
      log "Done"
