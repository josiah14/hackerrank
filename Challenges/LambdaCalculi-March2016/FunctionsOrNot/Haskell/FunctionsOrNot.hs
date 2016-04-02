import System.IO (readLn, getLine)
import Control.Monad (replicateM, mapM_, liftM)
import Data.List (nub)
import qualified Data.Map.Strict as M

data YesNo = YES | NO deriving Show

boolToYesNo :: Bool -> YesNo
boolToYesNo x = if x then YES else NO

readInt :: IO Int
readInt = readLn

tuplefy :: [a] -> (a, a)
tuplefy xs = case xs of [a, b] -> (a, b)
                        _      -> error "each item must consist of 2 values."

readPair :: [String] -> [(Int, Int)]
readPair = map $ tuplefy . map (read :: String -> Int) . words

getPairs :: Int -> IO [String]
getPairs = flip replicateM getLine

readTestCase :: Int -> IO [(Int, Int)]
readTestCase = fmap readPair . getPairs

readTestCases :: Int -> IO [[(Int, Int)]]
readTestCases = flip replicateM $ readInt >>= readTestCase

printAnswers :: Show a => [a] -> IO ()
printAnswers = mapM_ print

isSingletonSet :: Eq b => [b] -> Bool
isSingletonSet xs = case nub xs of [x] -> True
                                   _   -> False

listToMultiMap :: (Ord k, Eq k, Eq v) => [(k, v)] -> M.Map k [v]
listToMultiMap ((k, v):vs) = toMap (M.fromList [(k, [v])]) vs
    where toMap m xs = case xs of []        -> m
                                  (k0, v0):ys ->
                                      case M.lookup k0 m of Nothing -> toMap (M.insert k0 [v0] m) ys
                                                            Just v0s -> toMap (M.insert k0 (v0:v0s) m) ys

functionOrNot :: [(Int, Int)] -> YesNo
functionOrNot tuples = boolToYesNo $ foldl1 (&&) $ M.elems $ M.map isSingletonSet $ listToMultiMap tuples

main :: IO ()
main = getAnswers >>= printAnswers
    where getAnswers = (liftM . map) functionOrNot $ readInt >>= readTestCases
