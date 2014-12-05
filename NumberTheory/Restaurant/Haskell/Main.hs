import System.IO (readLn, getLine)
import Control.Monad (replicateM)

main :: IO ()
main = getNumLoaves >>= getLoafDimensions >>= mapM_ (print . minNumberOfSlices)

getNumLoaves :: IO Int
getNumLoaves = readLn

parseInt :: String -> Int
parseInt = read

tuplefy :: [a] -> (a, a)
tuplefy xs = case xs of [a, b] -> (a, b)
                        _      -> error "each line of input must consist of 2 integer values."

getLoafDimensions :: Int -> IO [(Int, Int)]
getLoafDimensions =
  let rawText         = flip replicateM getLine
      parseDimensions = map $ tuplefy . map parseInt . words
  in fmap parseDimensions . rawText

squares :: [Int]
squares = map (^2) [1..1000]

squareRoot :: Int-> Int
squareRoot squaredNum = floor . sqrt $ fromIntegral squaredNum

minNumberOfSlices :: (Int, Int) -> Int
minNumberOfSlices (l, b) =
  let area                        = l * b
      isPerfectSliceDimension num = area `rem` num + b `rem` squareRoot num + l `rem` squareRoot num == 0
      largestSquare               = last $ filter isPerfectSliceDimension $ take (min l b) squares
  in if l == b
     then 1
     else area `div` largestSquare

