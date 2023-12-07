import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import System.Environment (getArgs)

isSymbol :: Char -> Bool
isSymbol a =
  not (isDigit a) && (a /= '.')

safeChar :: String -> Int -> String
safeChar [] _ = ""
safeChar (x : _) 0 = if isDigit x then [x] else ""
safeChar (_ : xs) n = safeChar xs (n - 1)

isIndexSafe :: [String] -> (Int, Int) -> Maybe (Int, Int)
isIndexSafe lines (i, j) =
  if i >= 0 && i < length lines && j >= 0 && j < length (lines !! i)
    then Just (i, j)
    else Nothing

maybeArray :: Maybe [a] -> [a]
maybeArray Nothing = []
maybeArray (Just xs) = xs

getSymbolsPos :: [String] -> [(Int, Int)]
getSymbolsPos lines =
  [(i, j) | (i, row) <- zip [0 ..] lines, (j, cell) <- zip [0 ..] row, isSymbol cell]

isPosDigit :: [String] -> (Int, Int) -> Bool
isPosDigit lines (x, y) =
  isDigit ((lines !! x) !! y)

getAdjacentPos :: [String] -> (Int, Int) -> [(Int, Int)]
getAdjacentPos lines (x, y) =
  let isCharDigit = isPosDigit lines
      up = isIndexSafe lines (x - 1, y)
      down = isIndexSafe lines (x + 1, y)
      right = isIndexSafe lines (x, y + 1)
      left = isIndexSafe lines (x, y - 1)
      upRight = if isJust up && not (isCharDigit (fromJust up)) then isIndexSafe lines (x - 1, y + 1) else Nothing
      upLeft = if isJust up && not (isCharDigit (fromJust up)) then isIndexSafe lines (x - 1, y - 1) else Nothing
      downRight = if isJust down && not (isCharDigit (fromJust down)) then isIndexSafe lines (x + 1, y + 1) else Nothing
      downLeft = if isJust down && not (isCharDigit (fromJust down)) then isIndexSafe lines (x + 1, y - 1) else Nothing
      resultsRaw = [up, down, right, left, upRight, upLeft, downLeft, downRight]
      resultsJust = catMaybes resultsRaw
      results = filter (\(x, y) -> isDigit ((lines !! x) !! y)) resultsJust
   in results

getTotalNumber :: [String] -> Int -> (Int, Int) -> String
getTotalNumber lines 0 (x, y) =
  getTotalNumber lines (-1) (x, y) ++ getTotalNumber lines 1 (x, y)
getTotalNumber lines (-1) (x, y) =
  let digit = safeChar (lines !! x) (y - 1)
   in if digit == "" then "" else getTotalNumber lines (-1) (x, y - 1) ++ digit
getTotalNumber lines 1 (x, y) =
  let digit = safeChar (lines !! x) y
   in if digit == "" then "" else digit ++ getTotalNumber lines 1 (x, y + 1)

debugGetMatrixPos :: [String] -> [(Int, Int)] -> String
debugGetMatrixPos lines = map (\(i, j) -> (lines !! i) !! j)

main = do
  fileName <- getArgs
  content <- readFile (head fileName)
  let linesContent = lines content
      symbols = getSymbolsPos linesContent
      adjacentDigits = filter (not . null) (map (getAdjacentPos linesContent) symbols)
      -- cspell:ignore nums
      numsStr = map (getTotalNumber linesContent 0) (concat adjacentDigits)
      nums :: [Int]
      nums = map read numsStr
      result = sum nums
  -- mapM_ print symbols
  print nums
  print result
