module Types
  ( Byte,
    TokenList,
    Tape (..),
    Action (..),
    IMode (..),
  )
where

import Data.List (intercalate)

type Byte = Int

type TokenList = ([Char], Int)

data Tape = Tape
  { left :: [Byte],
    val :: Byte,
    right :: [Byte]
  }

data Action = Output | Input | Internal deriving (Eq, Show)

data IMode = Repl | Normal

byteS :: String -> String
byteS xs = case length xs of
  1 -> ' ' : (xs ++ " ")
  2 -> ' ' : xs
  _ -> xs

instance Show Tape where
  show (Tape l v r) =
    "Idx      : " ++ indexing ++ "\n"
      ++ "Byte Arr : "
      ++ l'
      ++ byteS (show v)
      ++ r'
      ++ newLinePt
    where
      l' =
        foldl
          (\acc x -> acc ++ "|" ++ byteS (show x))
          ""
          (reverse $ take 5 l)
          ++ "|"
      r' =
        "|"
          ++ tail
            ( foldl
                (\acc x -> acc ++ "|" ++ byteS (show x))
                ""
                (take 5 r)
                ++ "|"
            )
      labelLen = length "Byte Arr : "
      lLen = length l' + labelLen + 1
      lSpace = replicate lLen ' '
      newLinePt = "\n" ++ lSpace ++ "⬆"
      indexingList =
        ( (++ " ") . show <$> [-5 .. -1]
        )
          ++ ( (\x -> ' ' : (x ++ " ")) . show <$> [0 .. 5]
             )
      indexing = '|' : intercalate "|" indexingList ++ "|"