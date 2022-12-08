module Instructions where

import Parser
import Data.Char

type Code = [Instruction]

-- instructions for a simple machine
data Instruction
    = Copy Value Reg
    | Incr Reg
    | Decr Reg
    | JNZ Value Int
    deriving Show

-- a value is either a register or a number
data Value = Register Reg | Constant Int
    deriving Show

-- registers are named by lowercase letters
type Reg = Char

-- parse a string containing several instructions
-- (misparses are silently discarded)
parseCode :: String -> Code
parseCode = concatMap (parseAll instruction) . lines

instruction :: Parser Instruction
instruction =
    Copy <$ string "cpy" <* space <*> value <* space <*> reg <|>
    Incr <$ string "inc" <* space <*> reg <|>
    Decr <$ string "dec" <* space <*> reg <|>
    JNZ <$ string "jnz" <* space <*> value <* space <*> int

value :: Parser Value
value = Register <$> reg <|> Constant <$> int

reg :: Parser Reg
reg = satisfy isLower

testInput :: String
testInput = "\
    \cpy 1 a\n\
    \cpy 1 b\n\
    \cpy 26 d\n\
    \jnz c 2\n\
    \jnz 1 5\n\
    \cpy 7 c\n\
    \inc d\n\
    \dec c\n\
    \jnz c -2\n\
    \cpy a c\n\
    \inc a\n\
    \dec b\n\
    \jnz b -2\n\
    \cpy c b\n\
    \dec d\n\
    \jnz d -6\n\
    \cpy 19 c\n\
    \cpy 11 d\n\
    \inc a\n\
    \dec d\n\
    \jnz d -2\n\
    \dec c\n\
    \jnz c -5\n"