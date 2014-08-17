import System.Environment
import Control.Applicative
import Control.Monad
import Data.Word

-- Type definitions
newtype City a = City a

maxCities :: Word
maxCities = 100000

minCities :: Word
minCities = 2

toCity :: (Word a, Ord a) => a -> a -> City a
toCity city numCities
  | city < minCities = error "The number of cities cannot be less than " ++ (read minCities) ++ "."
  | city > maxCities = error "The number of cities cannot be greater than " ++ (read maxCities) ++ "."
  | otherwise = City city

fromCity :: City a -> a
fromCity (City city) = city


newtype Machine a = Machine a

maxMachines :: City Word -> Word
maxMachines = maxCities

minMachines :: Word
minMachines = minCities

toMachine :: (Word a, Ord a) => a -> a -> a -> Machine a
toMachine machine numMachines numCities
  | machine < minMachines = error "The number of machines cannot be less than " ++ (read minMachines) ++ "."
  | machine > maxMachines = error "The number of machines cannot be greater than " ++ (read maxMachines) ++ "."
  | otherwise = Machine machine

fromMachine :: Machine a -> a
fromMachine (Machine machine) = machine


newtype Road a = Road a

maxRoads :: City Word -> Word
maxRoads numCities = (fromCity numCities) - 1

minRoads :: Word
minRoads = minCities - 1

toRoad

-- Core logic

inputFile :: IO (FilePath)
inputFile = head <$> getArgs

outputFile :: IO (FilePath)
outputFile = flip (!!) 1 <$> getArgs

main :: IO ()
main = let content = readFile =<< inputFile
       in join $ writeFile <$> outputFile <*> content

parseInput :: String -> (N Word, K Word, )

