import Data.List (intercalate)
import Data.Text (intercalate, pack, replace, splitOn, strip, unpack)

data Colors = Red | Green | Blue

maxRed = 12

maxGreen = 13

maxBlue = 14

filterByNext :: [String] -> String -> [String]
filterByNext xs filterWord = [x | (x, y) <- zip xs (tail xs), isNumber x && y == filterWord]
  where
    isNumber s = case reads s :: [(Int, String)] of
      [(_, "")] -> True
      _ -> False

mult (a, b, c) =
  a * b * c

lineValue :: [Char] -> (Int, Int, Int)
lineValue line =
  let idAndGames = splitOn (pack ":") (pack line)
      id = read (unpack (last (splitOn (pack " ") (head idAndGames)))) :: Int
      games = map (unpack . strip) (splitOn (pack ";") (idAndGames !! 1))
      validGames = map gameValue games
      reds = map (\(a, _, _) -> a) validGames
      blues = map (\(_, a, _) -> a) validGames
      greens = map (\(_, _, a) -> a) validGames
      maxValid = (maximum reds, maximum blues, maximum greens)
   in maxValid

gameValue :: String -> (Int, Int, Int)
gameValue game =
  let colorsNumbersStr = map (unpack . strip) (splitOn (pack " ") (replace (pack ",") (pack "") (pack game)))
      reds = filterByNext colorsNumbersStr "red"
      blues = filterByNext colorsNumbersStr "blue"
      greens = filterByNext colorsNumbersStr "green"
      redsSum = if null reds then 0 else sum (map (\a -> read a :: Int) reds)
      bluesSum = if null blues then 0 else sum (map (\a -> read a :: Int) blues)
      greensSum = if null greens then 0 else sum (map (\a -> read a :: Int) greens)
   in (redsSum, bluesSum, greensSum)

main = do
  content <- readFile "day2.txt"
  let linesContent = lines content
      wordsContent = map (lineValue) linesContent
      wordsMult = map mult wordsContent
      sumResult = sum wordsMult
  mapM_ print wordsContent
  print sumResult