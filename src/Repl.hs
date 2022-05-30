module Repl
  ( repl,
  )
where

import Control.Monad (when)
import Interpreter (interpret)
import System.IO (hFlush, stdout)
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

count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count y (x : xs)
  | y == x = 1 + count y xs
  | otherwise = 0 + count y xs

handleLoop :: String -> IO String
handleLoop line
  | ctLB == ctRB = return line
  | ctLB > ctRB = do
    putStr "... "
    hFlush stdout
    line' <- getLine
    handleLoop (line ++ line')
  | otherwise = undefined
  where
    ctLB = count '[' line
    ctRB = count ']' line

repl :: Tape -> Bool -> IO ()
repl tp prompt = do
  when prompt $ putStr "==# "
  hFlush stdout
  s <- getLine >>= handleLoop
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