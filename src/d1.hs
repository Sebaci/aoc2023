module Main where

import           Data.Char                    (isAlpha, isDigit)
import           System.IO                    (readFile)
import           Text.ParserCombinators.ReadP

data LineChar = Digit Char | Other Char deriving (Show)
type Input = [[LineChar]]

main :: IO ()

main = do
    input <- parseInput <$> readFile "../input/d1.txt"
    print $ solveEasy input

solveEasy :: Input -> Int
solveEasy = sum . map extractNumber

extractNumber :: [LineChar] -> Int
extractNumber line =
    let onlyDigits = filter isDigitChar line
        [Digit d1, Digit d2] = [head onlyDigits, last onlyDigits]
    in
        read [d1, d2]
    where
        isDigitChar (Digit _) = True
        isDigitChar _         = False

parseInput :: String -> Input
parseInput text = case readP_to_S parseLines text of
    [(result, _)] -> result
    _             -> error "Parse error"

parseLines :: ReadP [[LineChar]]
parseLines = sepBy1 parseLine (char '\n') <* eof

parseLine :: ReadP [LineChar]
parseLine = many1 $ parseDigit <++ parseOther

parseDigit :: ReadP LineChar
parseDigit = Digit <$> satisfy isDigit

parseOther :: ReadP LineChar
parseOther = Other <$> satisfy isAlpha

