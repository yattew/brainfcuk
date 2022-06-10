-- brainfcuk to C transpiler to allow compilation of brainfcuk code
module Transpiler(
  generateCode
) where
rightShift :: String
rightShift = "pt++;"

leftShift :: String
leftShift = "pt--;"

putC :: String
putC = "putc(*pt,stdout);"

incrByte :: String
incrByte = "(*pt)++;"

decrByte :: String
decrByte = "(*pt)--;"

byteArr :: Int -> String
byteArr x =
  "char * pt = malloc(" ++ show (x * 8) ++ ");"
    ++ " memset(pt,0,"
    ++ show (x * 8)
    ++ ");"

boiler :: String -> String
boiler s =
  "#include<stdio.h>\n"
    ++ "#include<stdlib.h>\n"
    ++ "#include<string.h>\n"
    ++ "int main(){"
    ++ byteArr 100
    ++ s
    ++ "return 0;}"

startLoop :: String
startLoop =
  "while(*pt){"

endLoop :: String
endLoop = "}"

transpile :: String -> String
transpile [] = ""
transpile (x : xs)
  | x == '<' = leftShift ++ transpile xs
  | x == '>' = rightShift ++ transpile xs
  | x == '+' = incrByte ++ transpile xs
  | x == '-' = decrByte ++ transpile xs
  | x == '[' = startLoop ++ transpile xs
  | x == ']' = endLoop ++ transpile xs
  | x == '.' = putC ++ transpile xs
  | x == ',' = undefined
  | otherwise = transpile xs

generateCode :: String -> String
generateCode xs =
  boiler code
  where
    code = transpile xs