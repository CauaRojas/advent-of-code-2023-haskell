import Data.ByteString (elemIndex)
import Data.Char
import Data.List (sortBy)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (findIndex, pack, replace, unpack)
import Data.Text.Internal.Search (indices)
import Debug.Trace (traceShow)

-- cspell:ignore preprocess

splitIndices :: [(Int, [Int])] -> [(Int, [Int])]
splitIndices = concatMap (\(digit, indices) -> map (\index -> (digit, [index])) indices)

findAllIndices :: String -> String -> [Int]
findAllIndices needle haystack = map fromIntegral $ indices (pack needle) (pack haystack)

findNumbers :: String -> [(Int, [Int])]
findNumbers line =
  let searchValues = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
      indices = map (\(key, value) -> (read value :: Int, findAllIndices key line)) searchValues
   in indices

getNumber :: String -> Int
getNumber line =
  let searchValues = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
      digits = splitIndices $ map (\value -> (read value :: Int, findAllIndices value line)) searchValues
      handWrittenDigits = splitIndices $ findNumbers line
      allDigits = digits ++ handWrittenDigits
      nonEmptyDigits = filter (not . null . snd) allDigits
      sortedDigits = sortBy (\(_, a) (_, b) -> compare (head a) (head b)) nonEmptyDigits
   in if null sortedDigits
        then 0
        else read (show (fst (head sortedDigits)) ++ show (fst (last sortedDigits))) :: Int

main = do
  content <- readFile "day1.txt"
  let linesContent = lines content
      linesDigits = map getNumber linesContent
  mapM_ print linesDigits
  print (sum linesDigits)
