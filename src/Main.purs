module Main where

import Prelude

import Brainfuck.Binaryen (WasmCellSize, binaryToBuffer, cell0)
import Brainfuck.Compiler (compile)
import Brainfuck.IR (viewIR)
import Brainfuck.Optimizer (optimize)
import Brainfuck.Parser (parse)
import Brainfuck.Runtime (runBinary)
import Brainfuck.Transpiler (transpile)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), optional)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (now)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeFile)
import Node.Process (cwd)
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, help, helper, hsubparser, info, long, metavar, progDesc, short, strArgument, strOption, switch, (<**>))
import Pathy (parseAbsDir, parseRelFile, posixParser, posixPrinter, printPath, sandbox, (<.>))

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

type CommonOption r =
  ( inputFile :: String
  , noBinaryenOptimization :: Boolean
  , noBrainfuckOptimization :: Boolean
  , logOptimizedIR :: Boolean
  | r
  )

type CompileOption = CommonOption
  ( outputFile :: Maybe String
  )

type RunOption = CommonOption ()

data Option = RunOption (Record RunOption) | CompileOption (Record CompileOption)

commonOptionParser :: Parser (Record (CommonOption ()))
commonOptionParser = ado
  inputFile <- strArgument (metavar "INPUT_FILE")
  noBinaryenOptimization <- switch (long "no-binaryen-optimization" <> help "Disable Binaryen optimization")
  noBrainfuckOptimization <- switch (long "no-brainfuck-optimization" <> help "Disable Brainfuck optimization")
  logOptimizedIR <- switch (long "log-optimized-ir" <> help "Log optimized IR")
  in { inputFile, noBinaryenOptimization, noBrainfuckOptimization, logOptimizedIR }

compileOptionParser :: Parser (Record CompileOption)
compileOptionParser = ado
  common <- commonOptionParser
  outputFile <- optional $ strOption (short 'o' <> long "output" <> metavar "OUTPUT_FILE")
  in { inputFile: common.inputFile, noBinaryenOptimization: common.noBinaryenOptimization, noBrainfuckOptimization: common.noBrainfuckOptimization, logOptimizedIR: common.logOptimizedIR, outputFile }

runOptionParser :: Parser (Record RunOption)
runOptionParser = commonOptionParser

optionParser :: Parser Option
optionParser = hsubparser
  ( command "run" (info (RunOption <$> runOptionParser) (progDesc "Run Brainfuck code"))
      <> command "compile" (info (CompileOption <$> compileOptionParser) (progDesc "Compile Brainfuck code"))
  )

optionInfo :: ParserInfo Option
optionInfo = info (optionParser <**> helper) (fullDesc <> progDesc "Brainfuck optimizer & compiler")

main :: Effect Unit
main = launchAff_ do
  options <- liftEffect $ execParser optionInfo
  let
    { inputFile, noBinaryenOptimization, noBrainfuckOptimization, logOptimizedIR } = case options of
      RunOption o -> { inputFile: o.inputFile, noBinaryenOptimization: o.noBinaryenOptimization, noBrainfuckOptimization: o.noBrainfuckOptimization, logOptimizedIR: o.logOptimizedIR }
      CompileOption o -> { inputFile: o.inputFile, noBinaryenOptimization: o.noBinaryenOptimization, noBrainfuckOptimization: o.noBrainfuckOptimization, logOptimizedIR: o.logOptimizedIR }
  parsedInputFilePath <- case parseRelFile posixParser inputFile of
    Nothing -> liftEffect $ throw "Invalid input file path"
    Just pif -> pure pif
  currentWorkingDirectory <- liftEffect $ cwd
  log $ "Current working directory: " <> currentWorkingDirectory
  parsedCWD <- case parseAbsDir posixParser (currentWorkingDirectory <> "/") of
    Nothing -> liftEffect $ throw "Invalid current working directory"
    Just pcd -> pure pcd

  raw <- readTextFile UTF8 inputFile

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

  ir' /\ time <-
    if noBrainfuckOptimization then pure $ ir /\ 0.0
    else do
      log $ "Optimizing..."
      Milliseconds beforeOptimize <- unInstant <$> liftEffect now
      let
        ir' = optimize 3000 ir
      Milliseconds afterOptimize <- unInstant <$> liftEffect now
      log "Optimized"
      when logOptimizedIR do
        log $ viewIR ir'
      pure $ ir' /\ (afterOptimize - beforeOptimize)

  when logOptimizedIR do
    log $ viewIR ir'

  log "Compiling..."
  Milliseconds beforeCompile <- unInstant <$> liftEffect now
  binary <- liftEffect $
    compile
      { cellSize: config.cellSize, noBinaryenOptimization }
      ir'
  Milliseconds afterCompile <- unInstant <$> liftEffect now
  log "Compiled"

  case options of
    RunOption _ -> do
      log "Running..."
      log "== OUTPUT =="
      Milliseconds beforeRun <- unInstant <$> liftEffect now
      runBinary binary
      Milliseconds afterRun <- unInstant <$> liftEffect now
      log ""
      log "============"
      log $ "Parse time: " <> show (afterParse - beforeParse)
      log $ "Transpile time: " <> show (afterTranspile - beforeTranspile)
      when (not noBrainfuckOptimization) do
        log $ "Optimize time: " <> show time
      log $ "Compile time: " <> show (afterCompile - beforeCompile)
      log $ "Run time: " <> show (afterRun - beforeRun)
      log ""
      log $ "Total time: " <> show (afterRun - beforeParse)
      log "---------"
    CompileOption { outputFile } -> do
      buf <- liftEffect $ binaryToBuffer binary
      sandboxedoutputPath <- case sandbox parsedCWD (parsedInputFilePath <.> "wasm") of
        Nothing -> liftEffect $ throw "Unsafe path"
        Just s -> pure s
      let
        fixedOutputFile = case outputFile of
          Just o -> o
          Nothing -> printPath posixPrinter sandboxedoutputPath
      writeFile fixedOutputFile buf
  log "Done"
