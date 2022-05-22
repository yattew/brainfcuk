import Data.Char (chr, ord)

type Byte = Int

data Tape a = Tape {tape :: [a], iterator :: Int} deriving (Show)

type BTape = Tape Byte

type CTape = Tape Char

data Action = Output | Input | Internal deriving (Eq, Show)

getByte :: BTape -> Byte
getByte (Tape arr pt) = arr !! pt

putByte :: Byte -> BTape -> BTape
putByte b (Tape arr pt) = Tape arrModified pt
  where
    lh = take pt arr
    rh = drop pt arr
    arrModified = lh ++ b : rh

changeByte :: Int -> BTape -> BTape
changeByte x (Tape arr pt) = Tape arrModified pt
  where
    lh = take pt arr
    rh = drop (pt + 1) arr
    -- currByte = if arr !! pointer == 255 &&
    arrModified = lh ++ ((arr !! pt) + x) : rh

mvIterator :: Int -> Tape a -> Tape a
mvIterator x (Tape tp it) = Tape tp (it + x)

skipToEnd :: Int -> CTape -> CTape
skipToEnd x tp
  | x == 0 = mvIterator (-1) tp
  | tarr !! it == '[' = skipToEnd (x + 1) (mvIterator 1 tp)
  | tarr !! it == ']' = skipToEnd (x - 1) (mvIterator 1 tp)
  | otherwise = skipToEnd x (mvIterator 1 tp)
  where
    (Tape tarr it) = tp

skipToStart :: Int -> CTape -> CTape
skipToStart x tp
  | x == 0 = mvIterator 1 tp
  | tarr !! it == ']' = skipToStart (x + 1) (mvIterator (-1) tp)
  | tarr !! it == '[' = skipToStart (x -1) (mvIterator (-1) tp)
  | otherwise = skipToStart x (mvIterator (-1) tp)
  where
    (Tape tarr it) = tp

interpretToken :: (CTape, BTape) -> (Action, CTape, BTape)
interpretToken (tp, bArr)
  | tarr !! it == '+' = (Internal, tp, changeByte 1 bArr)
  | tarr !! it == '-' = (Internal, tp, changeByte (-1) bArr)
  | tarr !! it == '>' = (Internal, tp, mvIterator 1 bArr)
  | tarr !! it == '<' = (Internal, tp, mvIterator (-1) bArr)
  | tarr !! it == '.' = (Output, tp, bArr)
  | tarr !! it == ',' = (Input, tp, bArr)
  | tarr !! it == '[' =
    if arr !! pt == 0
      then (Internal, skipToEnd 1 (mvIterator 1 tp), bArr)
      else (Internal, tp, bArr)
  | tarr !! it == ']' =
    if arr !! pt == 0
      then (Internal, tp, bArr)
      else (Internal, skipToStart 1 (mvIterator (-1) tp), bArr)
  | otherwise = (Internal, tp, bArr)
  where
    (Tape tarr it) = tp
    (Tape arr pt) = bArr

interpretTape :: CTape -> BTape -> IO ()
interpretTape tp bArr
  | iterator tp < length (tape tp) = do
    let (action, tp', bArr') = interpretToken (tp, bArr)
        tp'' = mvIterator 1 tp'
    case action of
      Output -> do
        putChar (chr (getByte bArr'))
        interpretTape tp'' bArr'
      Input -> do
        c <- fmap head getLine
        let bArr'' = putByte (ord c) bArr'
        interpretTape tp'' bArr''
      Internal -> interpretTape tp'' bArr'
  | otherwise = return ()

runInterpreter :: String -> IO ()
runInterpreter s = interpretTape ctp btp
  where
    ctp = Tape s 0
    btp = Tape (replicate 100 0) 0