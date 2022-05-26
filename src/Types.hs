module Types
  ( Byte,
    TokenList,
    Tape (..),
    Action (..),
    IMode (..),
  )
where

type Byte = Int

type TokenList = ([Char], Int)

data Tape = Tape
  { left :: [Byte],
    val :: Byte,
    right :: [Byte]
  }

data Action = Output | Input | Internal deriving (Eq, Show)

data IMode = Repl | Normal

instance Show Tape where
  show (Tape l v r) = l' ++ show v ++ r' ++ newLinePt
    where
      l' = foldl (\acc x -> acc ++ "|" ++ show x) "" (reverse $ take 5 l) ++ "|"
      r' = foldl (\acc x -> acc ++ "|" ++ show x) "" (take 5 r) ++ "|"
      lLen = length l'
      newLinePt = "\n" ++ replicate lLen ' ' ++ "^"