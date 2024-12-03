module Day3 (run) where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Instr
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq)

type Parser = Parsec Void String

-- Parse 1-3 digits
number :: Parser Int
number = read <$> count' 1 3 digitChar

-- Parse multiplication
parseMul :: Parser Instr
parseMul = do
  _ <- string "mul("
  n1 <- number
  _ <- char ','
  n2 <- number
  _ <- char ')'
  return $ Mul n1 n2

-- Parse do instruction
parseDo :: Parser Instr
parseDo = Do <$ string "do()"

-- Parse dont instruction
parseDont :: Parser Instr
parseDont = Dont <$ string "don't()"

-- Parse a single instruction or skip a character
parseChunk :: Parser (Maybe Instr)
parseChunk =
  (Just <$> try parseMul)
    <|> (Just <$> try parseDo)
    <|> (Just <$> try parseDont)
    <|> (Nothing <$ anySingle)

-- Parse entire input
parseInput :: Parser [Instr]
parseInput = catMaybes <$> many parseChunk <* eof

type ControlState = (Bool, Int)

control :: [Instr] -> Control.Monad.State.State ControlState Int
control [] = do
  (_, acc) <- get
  return acc
control (x : xs) = do
  (on, acc) <- get
  case x of
    Do -> put (True, acc)
    Dont -> put (False, acc)
    Mul a b | on -> put (True, acc + a * b)
    Mul _ _ -> put (on, acc)
  control xs

run :: String -> IO ()
run input = do
  content <- readFile input
  -- let content = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  case runParser parseInput "input" content of
    Left err -> putStrLn $ errorBundlePretty err
    Right instrs -> do
      print instrs
      print $ sum [a * b | Mul a b <- instrs]
      print $ evalState (control instrs) (True, 0)
