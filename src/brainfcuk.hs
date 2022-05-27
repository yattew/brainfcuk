import Control.Monad (when)
import Interpreter (interpret)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdout)
import Types (IMode (..), Tape (Tape))

initTape :: Tape
initTape = Tape (repeat 0) 0 (repeat 0)

repl :: Tape -> Bool -> IO ()
repl tp prompt = do
  when prompt $ putStr "==# "
  hFlush stdout
  s <- getLine
  let prompt' = ',' `notElem` s
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
        _ -> do
          tp' <- interpret Repl (s, 0) tp
          repl tp' prompt'
    else do
      tp' <- interpret Repl (s, 0) tp
      repl tp' prompt'

showHelp :: IO ()
showHelp = undefined

cli :: IO ()
cli = do
  args <- getArgs
  case length args of
    0 -> repl tp True
    1 -> do
      let fileName = head args
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

main :: IO ()
main = do
  cli