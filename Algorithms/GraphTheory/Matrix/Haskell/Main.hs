import System.Environment
import Control.Applicative
import Control.Monad
import Data.Word
import Data.List.Split
import qualified Data.Text as T
import Development.Placeholders

-- Type definitions
newtype CityCount a = CityCount Word

maxCities :: Word
maxCities = 100000

minCities :: Word
minCities = 2

toCityCount :: Word -> (CityCount Word)
toCityCount count
  | count < minCities = error $ "The number of cities cannot be less than " ++ (show minCities) ++ "."
  | count > maxCities = error $ "The number ofd cities cannot be greater than " ++ (show maxCities) ++ "."
  | otherwise = CityCount count

fromCityCount :: CityCount Word -> Word
fromCityCount (CityCount count) = count


newtype MachineCount a = MachineCount Word

minMachines :: Word
minMachines = minCities

toMachineCount :: Word -> CityCount Word -> MachineCount Word
toMachineCount count (CityCount numCities)
  | count < minMachines = error $ "The number of machines cannot be less than " ++ (show minMachines) ++ "."
  | count > numCities = error "The number of machines cannot exceed the number of cities."
  | otherwise = MachineCount count

fromMachineCount :: MachineCount Word -> Word
fromMachineCount (MachineCount count) = count


type City = Word
type Cities = [City]

type Machine = Word
type Machines = [Machine]


newtype RoadDestroyTime a = RoadDestroyTime Word

minDestroyTime :: Word
minDestroyTime = 1

maxDestroyTime :: Word
maxDestroyTime = 1000000

toRoadDestroyTime :: Word -> RoadDestroyTime Word
toRoadDestroyTime time
  | time < minDestroyTime = error $ "The destroy time for a road cannot be less than " ++ (show minDestroyTime) ++ "."
  | time > maxDestroyTime = error $ "The max destroy time for a road cannot be greater than " ++ (show maxDestroyTime)
                              ++ "."
  | otherwise = RoadDestroyTime time


data Road = Road { city0 :: City
                 , city1 :: City
                 , destroyTime :: RoadDestroyTime Word
                 }

toRoad :: (City, City, RoadDestroyTime Word) -> Road
toRoad (city0, city1, destroyTime) = Road city0 city1 destroyTime

fromRoad :: Road -> (City, City, RoadDestroyTime Word)
fromRoad (Road city0 city1 destroyTime) = (city0, city1, destroyTime)

type Roads = [Road]

maxRoads :: CityCount Word -> Word
maxRoads numCities = (fromCityCount numCities) - 1

minRoads :: Word
minRoads = minCities - 1

-- Core logic

inputFile :: IO (FilePath)
inputFile = head <$> getArgs

outputFile :: IO (FilePath)
outputFile = flip (!!) 1 <$> getArgs

main :: IO ()
main = let content = readFile =<< inputFile
       in join $ writeFile <$> outputFile <*> content

parseInput' :: String -> (CityCount Word, MachineCount Word, Cities, Roads, Machines)
parseInput' input = (toCityCount 3, toMachineCount 2 (toCityCount 3), [0, 1, 2], map toRoad [(0, 1, toRoadDestroyTime 1), (0, 2, toRoadDestroyTime 5)], [0, 2])

splitLines :: T.Text -> [T.Text]
splitLines = (map T.strip) . (T.splitOn $ T.pack "\n")

splitNumbers :: [T.Text] -> [[T.Text]]
splitNumbers = map $ T.splitOn $ T.pack " "

parseInput :: [[T.Text]] -> (CityCount Word, MachineCount Word, Cities, Roads, Machines)
parseInput = error undefined

