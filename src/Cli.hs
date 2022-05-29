module Cli
  ( cli,
  )
where

import Control.Monad (when)
import Interpreter (interpret)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdout)
import Types (IMode (..), Tape (Tape))

initTape :: Tape
initTape = Tape (repeat 0) 0 (repeat 0)

replHelp :: IO ()
replHelp = do
  putStrLn "Repl Commands :"
  putStrLn "    :help          => display this help"
  putStrLn "    :quit          => quit the repl"
  putStrLn "    :tape          => print the byte array"
  putStrLn "    :reset         => reset the byte array to 0"
  putStrLn "    :load <name>   => interpret file provided as an argument"
  putChar '\n'

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

repl :: Tape -> Bool -> IO ()
repl tp prompt = do
  when prompt $ putStr "==# "
  hFlush stdout
  s <- getLine
  let prompt' = ',' `notElem` s
  if null s
    then repl tp prompt'
    else
      if head s == ':'
        then do
          let cmd = takeWhile (/= ' ') s
              arg = dropWhile (/= ' ') s
          case cmd of
            ":quit" -> return ()
            ":tape" -> do
              print tp
              repl tp prompt'
            ":reset" -> do
              repl initTape prompt'
            ":load" -> do
              let fileName = tail arg
              tokens <- readFile fileName
              tp' <- interpret Repl (tokens, 0) initTape
              repl tp' prompt'
            ":help" -> do
              putChar '\n'
              putStrLn "######## Help ########"
              replHelp
              repl tp prompt'
            _ -> do
              tp' <- interpret Repl (s, 0) tp
              repl tp' prompt'
        else do
          tp' <- interpret Repl (s, 0) tp
          repl tp' prompt'

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
    tp = initTape