import Interpreter (interpret)
import System.Directory.Internal.Prelude (getArgs)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdout)
import Types (IMode (..), Tape (Tape))

initTape :: Tape
initTape = Tape (repeat 0) 0 (repeat 0)

repl :: Tape -> IO ()
repl tp = do
  putStr ">>>"
  hFlush stdout
  s <- getLine
  case s of
    ":quit" -> return ()
    ":tape" -> do
      print tp
      repl tp
    ":clear" -> do
      repl initTape
    ":load" -> undefined
    _ -> do
      tp' <- interpret Repl (s, 0) tp
      repl tp'

showHelp :: IO ()
showHelp = undefined

cli :: IO ()
cli = do
  args <- getArgs
  case length args of
    0 -> repl tp
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
          repl tp'
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