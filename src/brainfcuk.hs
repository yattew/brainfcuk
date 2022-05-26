import Interpreter (interpret)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdout)
import Types (IMode (Repl), Tape (Tape))

initTape :: Tape
initTape = Tape (repeat 0) 0 (repeat 0)

repl :: Tape -> IO ()
repl tp = do
  putStr ">>"
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

cli :: IO ()
cli = repl tp
  where
    tp = initTape

main :: IO ()
main = do
  cli