import Control.Monad (forM_, when)
import Data.Char (chr, ord)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

--Tape is used as a list of input characters and as the byte array
data Tape a = Tape {tape :: [a], iterator :: Int} deriving (Show)

--Action describes the kind of action taken by the interpreter when interpreting a token
--This makes the IO portion of the interpreter seperate from the token interpreter
data Action = Output | Input | Internal deriving (Eq, Show)

type Byte = Int

type BTape = Tape Byte

type CTape = Tape Char

-- get the current byte the pointer is pointing at
getByte :: BTape -> Byte
getByte (Tape arr pt) = arr !! pt

-- put a byte b where the pointer is pointing at
putByte :: Byte -> BTape -> BTape
putByte b (Tape arr pt) = Tape arrModified pt
  where
    lh = take pt arr
    rh = drop (pt + 1) arr
    arrModified = lh ++ b : rh

-- +x or -x the current byte 
changeByte :: Int -> BTape -> BTape
changeByte x (Tape arr pt) = Tape arrModified pt
  where
    lh = take pt arr
    rh = drop (pt + 1) arr
    arrModified = lh ++ ((arr !! pt) + x) : rh

-- move the iterator forward or backward in any of the tapes
mvIterator :: Int -> Tape a -> Tape a
mvIterator x (Tape ctp it) = Tape ctp (it + x)

-- skip to the corresponding ']' when the current byte is 0 on reaching '['
skipToEnd :: Int -> CTape -> CTape
skipToEnd x ctp
  | x == 0 = mvIterator (-1) ctp
  | char == '[' = skipToEnd (x + 1) (mvIterator 1 ctp)
  | char == ']' = skipToEnd (x - 1) (mvIterator 1 ctp)
  | otherwise = skipToEnd x (mvIterator 1 ctp)
  where
    (Tape tarr it) = ctp
    char = tarr !! it

-- skip to the starting '[' when the current byte is not 0 on reaching ']'
skipToStart :: Int -> CTape -> CTape
skipToStart x ctp
  | x == 0 = mvIterator 1 ctp
  | char == ']' = skipToStart (x + 1) (mvIterator (-1) ctp)
  | char == '[' = skipToStart (x -1) (mvIterator (-1) ctp)
  | otherwise = skipToStart x (mvIterator (-1) ctp)
  where
    (Tape tarr it) = ctp
    char = tarr !! it

--interpret the current token/character in the ctp and return new ctp and btp with the action performed
interpretToken :: (CTape, BTape) -> (Action, CTape, BTape)
interpretToken (ctp, btp)
  | char == '+' = (Internal, ctp, changeByte 1 btp)
  | char == '-' = (Internal, ctp, changeByte (-1) btp)
  | char == '>' = (Internal, ctp, mvIterator 1 btp)
  | char == '<' = (Internal, ctp, mvIterator (-1) btp)
  | char == '.' = (Output, ctp, btp)
  | char == ',' = (Input, ctp, btp)
  | char == '[' =
    if barr !! pt == 0
      then (Internal, skipToEnd 1 (mvIterator 1 ctp), btp)
      else (Internal, ctp, btp)
  | carr !! it == ']' =
    if barr !! pt == 0
      then (Internal, ctp, btp)
      else (Internal, skipToStart 1 (mvIterator (-1) ctp), btp)
  | otherwise = (Internal, ctp, btp)
  where
    (Tape carr it) = ctp
    (Tape barr pt) = btp
    char = carr !! it

-- recursive interpretation of the input char tape taking a initial byte tape as the second argument
interpretTape :: CTape -> BTape -> IO ()
interpretTape ctp btp
  | iterator ctp < length (tape ctp) = do
    let (action, ctp', btp') = interpretToken (ctp, btp)
        ctp'' = mvIterator 1 ctp'
    case action of
      Output -> do
        putChar $chr (getByte btp')
        interpretTape ctp'' btp'
      Input -> do
        c <- getChar
        let btp'' = putByte (ord c) btp'
        interpretTape ctp'' btp''
      Internal -> interpretTape ctp'' btp'
  | otherwise = return ()

runInterpreter :: String -> IO ()
runInterpreter s =
  interpretTape ctp btp
  where
    btp = Tape (replicate 100 0) 0
    ctp = Tape s 0

handleCli :: IO (Maybe String)
handleCli = do
  args <- getArgs
  if head args == "stdin"
    then fmap Just getContents
    else fmap Just (readFile (head args))

main :: IO ()
main = do
  handleCli >>= (`forM_` runInterpreter)