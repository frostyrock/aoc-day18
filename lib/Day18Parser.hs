module Day18Parser
  ( loadProgram
  , Instruction(..)
  , Operand(..)
  ) where

import Text.ParserCombinators.Parsec

data Instruction =
    Snd Operand
  | Rcv Operand
  | Add Operand Operand
  | Mul Operand Operand
  | Mod Operand Operand
  | Set Operand Operand
  | Jgz Operand Operand
    deriving (Eq, Show)

data Operand = I Int | C Char
    deriving (Eq, Show)


instructionP :: Parser Instruction
instructionP = (try addP)
    <|> (try mulP)
    <|> (try modP)
    <|> (try setP)
    <|> (try jgzP)
    <|> (try sndP)
    <|> rcvP

loadProgram :: String -> Either ParseError [Instruction]
loadProgram str = parse programP "" str

programP :: Parser [Instruction]
programP = do
  first <- instructionP
  rest  <- (space >> programP) <|> return []
  return $ first : rest

numP :: Parser Operand
numP = do
  sign  <- option id ( char '-' >> return negate)
  digits <- (many1 digit)
  return . I . sign . read $ digits

chrP :: Parser Operand
chrP =  oneOf ['a'..'z'] >>= return . C

opP :: Parser Operand
opP = try numP <|> chrP

unaOpP :: (Operand -> Instruction)
  -> String
  -> Parser Instruction
unaOpP ctor str = do
  x <- string str >> space >> chrP
  return . ctor  $ x

sndP :: Parser Instruction
sndP = unaOpP Snd "snd"

rcvP :: Parser Instruction
rcvP = unaOpP Rcv "rcv"

binOpP :: (Operand -> Operand -> Instruction)
 -> String
 -> Parser Instruction
binOpP ctor str = do
  x <- string str >> space >> chrP
  y <- space >> opP
  return $ ctor x y

addP :: Parser Instruction
addP = binOpP Add "add"

mulP :: Parser Instruction
mulP = binOpP Mul "mul"

modP :: Parser Instruction
modP = binOpP Mod "mod"

setP :: Parser Instruction
setP = binOpP Set "set"

jgzP :: Parser Instruction
jgzP = do
  x <- string "jgz" >> space >> opP
  y <- space >> opP
  return $ Jgz x y