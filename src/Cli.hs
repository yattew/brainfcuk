module Cli
  ( cli,
  )
where

import Interpreter (interpret)
import Repl (repl)
import System.Environment (getArgs)
import System.IO (hPutStr)
import Types (IMode (..), Tape (Tape))

showHelp :: IO ()
showHelp = do
  putStrLn "###### BrainFcuk ######\n"
  putStrLn "Usage :"
  putStrLn "    brainfcuk"
  putStrLn "         |start the brainfcuk repl\n"
  putStrLn "    brainfcuk <name>"
  putStrLn "    brainfcuk -f <name>"
  putStrLn "         | interpret the file provided as an argument\n"
  putStrLn "    brainfcuk -i <name>"
  putStrLn "         | interpret file and start the repl in the same context of as the file\n"
  putStrLn "use \":help\" in the repl to see repl usage"
  putChar '\n'

cli :: IO ()
cli = do
  args <- getArgs
  case length args of
    0 -> do
      putStrLn "use :help to show help"
      repl tp True
    1 -> do
      let arg = head args
      case arg of
        "-h" -> showHelp
        _ -> do
          let fileName = arg
          tokens <- readFile fileName
          interpret Normal (tokens, 0) tp
          return ()
    2 -> do
      let mode = head args
          fileName = args !! 1
      case mode of
        "-i" -> do
          tokens <- readFile fileName
          tp' <- interpret Normal (tokens, 0) tp
          repl tp' True
        "-f" -> do
          let fileName = head args
          tokens <- readFile fileName
          interpret Normal (tokens, 0) tp
          return ()          
        _ -> putStrLn "Invalid Usage use -h to see help"
    _ -> putStrLn "Invalid Usage use -h to see help"
  where
    tp = Tape (repeat 0) 0 (repeat 0)