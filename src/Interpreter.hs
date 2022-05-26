module Interpreter
  ( interpret,
  )
where

import Data.Char (chr, ord)
import Types (Action (..), Byte, IMode (..), Tape (Tape), TokenList)

lShift :: Tape -> Tape
lShift (Tape l v r) = Tape l' v' r'
  where
    (v' : l') = l
    r' = v : r

rShift :: Tape -> Tape
rShift (Tape l v r) = Tape l' v' r'
  where
    (v' : r') = r
    l' = v : l

updateByte :: Tape -> Byte -> Tape
updateByte (Tape l v r) b = Tape l b r

incrByte :: Tape -> Tape
incrByte (Tape l v r) = Tape l v' r
  where
    v' =
      if v + 1 > 255
        then 0
        else v + 1

decrByte :: Tape -> Tape
decrByte (Tape l v r) = Tape l v' r
  where
    v' =
      if v - 1 < 0
        then 255
        else v -1

currByte :: Tape -> Byte
currByte (Tape l v r) = v

outputByte :: Byte -> IO ()
outputByte b = putChar $ chr b

skipToEnd :: TokenList -> Int -> TokenList
skipToEnd (tokens, it) n
  | n == 0 = (tokens, it - 1)
  | char == '[' = skipToEnd (tokens, it + 1) (n + 1)
  | char == ']' = skipToEnd (tokens, it + 1) (n -1)
  | otherwise = skipToEnd (tokens, it + 1) n
  where
    char = tokens !! it

skipToStart :: TokenList -> Int -> TokenList
skipToStart (tokens, it) n
  | n == 0 = (tokens, it + 1)
  | char == ']' = skipToStart (tokens, it - 1) (n + 1)
  | char == '[' = skipToStart (tokens, it - 1) (n -1)
  | otherwise = skipToStart (tokens, it - 1) n
  where
    char = tokens !! it

interpretToken :: TokenList -> Tape -> (Action, TokenList, Tape)
interpretToken tokenList tp = case token of
  '<' -> (Internal, tokenList, lShift tp)
  '>' -> (Internal, tokenList, rShift tp)
  '+' -> (Internal, tokenList, incrByte tp)
  '-' -> (Internal, tokenList, decrByte tp)
  '[' ->
    ( Internal,
      if currByte tp == 0
        then skipToEnd (tokens, it + 1) 1
        else tokenList,
      tp
    )
  ']' ->
    ( Internal,
      if currByte tp /= 0
        then skipToStart (tokens, it - 1) 1
        else tokenList,
      tp
    )
  '.' -> (Output, tokenList, tp)
  ',' -> (Input, tokenList, tp)
  _ -> (Internal, tokenList, tp)
  where
    (tokens, it) = tokenList
    token = tokens !! it

interpret :: IMode -> TokenList -> Tape -> IO Tape
interpret mode tokenList tp
  | it < length tokens =
    do
      let (action, (tokens', it'), tp') = interpretToken tokenList tp
          tokenList' = (tokens', it' + 1)
      case action of
        Output -> do
          outputByte (currByte tp')
          interpret mode tokenList' tp'
        Input -> case mode of
          Repl -> do
            c <- fmap head getLine
            let tp'' = updateByte tp' (ord c)
            interpret mode tokenList' tp''
          Normal -> do
            c <- getChar
            let tp'' = updateByte tp' (ord c)
            interpret mode tokenList' tp''
        Internal -> interpret mode tokenList' tp'
  | otherwise = return tp
  where
    (tokens, it) = tokenList
