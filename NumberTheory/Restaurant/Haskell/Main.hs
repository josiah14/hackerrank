import System.IO

getNumLoaves :: IO Int
getNumLoaves = readLn

parseInt :: String -> Int
parseInt str = read str

tuplefy :: [a] -> (a, a)
tuplefy xs = case xs of [a, b] -> (a, b)
                        _      -> error "each line of input must consist of 2 integer values."

main :: IO ()
main = do
  numLoaves <- getNumLoaves
  loafDimensions <- sequence $ take numLoaves $ repeat getLine
  let parsedDims = map (\str -> tuplefy $ map parseInt $ words str) loafDimensions
  mapM_ print $ map largestSide parsedDims

squares :: [Int]
squares = map (^2) [1..1000]

squareRoot :: Int-> Int
squareRoot squaredNum = floor . sqrt $ fromIntegral $ squaredNum

largestSide :: (Int, Int) -> Int
largestSide (l, b) =
  let area = l * b
      largestSquare =
        last $ filter (\num -> area `rem` num + b `rem` squareRoot num + l `rem` squareRoot num == 0)
                      $ take (min l b) squares
  in if l == b then 1 else area `div` largestSquare

