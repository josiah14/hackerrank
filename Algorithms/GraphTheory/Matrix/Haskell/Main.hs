import System.Environment
import Control.Applicative
import Control.Monad
import Data.Word
import Data.List.Split
import qualified Data.Text as T
import Development.Placeholders
import Data.List as L

-- Type definitions
newtype CityCount a = CityCount Int deriving (Show, Eq, Ord)

maxCities :: Int
maxCities = 100000

minCities :: Int
minCities = 2

toCityCount :: Int -> CityCount Int
toCityCount count
  | count < minCities = error $ "The number of cities cannot be less than " ++ show minCities ++ "."
  | count > maxCities = error $ "The number of cities cannot be greater than " ++ show maxCities ++ "."
  | otherwise = CityCount count

fromCityCount :: CityCount Int -> Int
fromCityCount (CityCount count) = count


newtype MachineCount a = MachineCount Int deriving (Show, Eq, Ord)

minMachines :: Int
minMachines = minCities

toMachineCount :: Int -> CityCount Int -> MachineCount Int
toMachineCount count (CityCount numCities)
  | count < minMachines = error $ "The number of machines cannot be less than " ++ show minMachines ++ "."
  | count > numCities = error "The number of machines cannot exceed the number of cities."
  | otherwise = MachineCount count

fromMachineCount :: MachineCount Int -> Int
fromMachineCount (MachineCount count) = count


type City = Int
type Cities = [City]

newtype Machine a = Machine Int deriving (Show, Eq, Ord)

toMachine :: CityCount Int -> Int -> Machine Int
toMachine (CityCount numCities) machine
  | machine < 0 = error "Negative numbers are not allowed for a machine's resident city."
  | machine > numCities - 1 = error $ "The max number of cities is " ++ show numCities
                                      ++ " but the resident city of the machine is outside that bound at "
                                      ++ show machine ++ "."
  | otherwise = Machine machine

fromMachine :: Machine Int -> Int
fromMachine (Machine machine) = machine


newtype Machines a = Machines [Machine Int] deriving (Show, Eq, Ord)

toMachines :: (MachineCount Int) -> [Machine Int] -> Machines [Machine Int]
toMachines (MachineCount count) machineList
  | length machineList /= fromIntegral count = error "The number of machines specified does not match the number provided."
  | otherwise = Machines machineList

fromMachines :: Machines [Machine Int] -> [Int]
fromMachines (Machines machines) = map fromMachine machines

newtype RoadDestroyTime a = RoadDestroyTime Int deriving (Show, Eq, Ord)

minDestroyTime :: Int
minDestroyTime = 1

maxDestroyTime :: Int
maxDestroyTime = 1000000

toRoadDestroyTime :: Int -> RoadDestroyTime Int
toRoadDestroyTime time
  | time < minDestroyTime = error $ "The destroy time for a road cannot be less than " ++ show minDestroyTime ++ "."
  | time > maxDestroyTime = error $ "The max destroy time for a road cannot be greater than " ++ show maxDestroyTime
                              ++ "."
  | otherwise = RoadDestroyTime time

fromRoadDestroyTime :: RoadDestroyTime Int -> Int
fromRoadDestroyTime (RoadDestroyTime time) = time


data Road = Road { cities :: (City, City)
                 , destroyTime :: RoadDestroyTime Int
                 } deriving Show

toRoad :: ((City, City), RoadDestroyTime Int) -> Road
toRoad (cities, destroyTime) = Road cities destroyTime

fromRoad :: Road -> ((City, City), RoadDestroyTime Int)
fromRoad (Road cities destroyTime) = (cities, destroyTime)


newtype Roads a = Roads [Road] deriving Show

numRoads :: CityCount Int -> Int
numRoads numCities = fromCityCount numCities - 1

toRoads :: (CityCount Int) -> [Road] -> Roads [Road]
toRoads cityCount roads
  | length roads /= numRoads cityCount = error "The number of roads must be 1 less than the number of total cities."
  | otherwise = Roads roads

fromRoads :: Roads [Road] -> [Road]
fromRoads (Roads roads) = roads

newtype KingdomTree a = KingdomTree [(City, [Road])]

-- This creates a traversable tree of the Kingdom's city grid.
-- The data formate is an array of 2 element tuples with the
-- representations as [(City, roadsConnectedToCity)]
toKingdomTree :: Roads [Road] -> KingdomTree [(City, [Road])]
toKingdomTree (Roads roads) = KingdomTree $ buildTree 0 roads -- start with city 0 every time.
  where buildTree city roads =
          let (connectedRoads, unconnectedRoads) = L.partition (connects city) roads
          in if null connectedRoads then []
             else (city, connectedRoads) -- first element of the returned list
                  :(concat $ map (\road -> buildTree (otherCity road) $ road:unconnectedRoads) connectedRoads) -- the rest of the returned list
          where otherCity (Road cities _) = if city == fst cities then snd cities else fst cities
                connects c (Road cityPair _) = c == fst cityPair || c == snd cityPair

-- This rotates the traversable tree of the Kingdom's city grid such that
-- the city passed in becomes the root node of the entire tree.
rotateKingdom :: KingdomTree [(City, City, [Road])] -> City -> KingdomTree [(City, City, [Road])]
rotateKingdom kingdom city = kingdom

-- Core logic

inputFile :: IO FilePath
inputFile = head <$> getArgs

outputFile :: IO FilePath
outputFile = flip (!!) 1 <$> getArgs

main :: IO ()
main = let content = readFile =<< inputFile
       in join $ writeFile <$> outputFile <*> (show . parseInput <$> content)

splitLines :: T.Text -> [T.Text]
splitLines = filter (T.pack "" /=) . map T.strip . T.splitOn (T.pack "\n")

splitNumbers :: [T.Text] -> [[T.Text]]
splitNumbers = map $ T.splitOn $ T.pack " "

readNumbers :: [[T.Text]] -> [[Int]]
readNumbers = map $ map $ read . T.unpack

parseInput :: String -> (CityCount Int, MachineCount Int, Cities, Roads [Road], Machines [Machine Int])
parseInput st = let parsedInput = readNumbers . splitNumbers . splitLines . T.pack $ st
                    counts = head parsedInput
                    cityCount = toCityCount $ head counts
                    machineCount = toMachineCount (last counts) cityCount
                    cities = [0..fromCityCount cityCount - 1]
                    roads = toRoads cityCount
                            $ map toRoad
                            $ map (\triple -> ((head triple, triple!!1), toRoadDestroyTime $ triple!!2))
                                  (tail (take (fromCityCount cityCount) parsedInput))
                    machines = toMachines machineCount
                               $ map (toMachine cityCount)
                               $ sort $ flatten $ drop (fromCityCount cityCount) parsedInput
                in (cityCount, machineCount, cities, roads, machines)
                where flatten = map head

-- findPath :: City -> City -> Roads [Road] -> [((City, City), Int)]
-- findPath startCity endCity cityGrid = let roads = map (\(cities, time) -> (cities, fromRoadDestroyTime time))
--                                             $ map fromRoad $ fromRoads cityGrid
--                                 in roads
--                                 where linksCity0 r = let c1 = fst $ fst r
--                                                          c2 = snd $ fst r
--                                                      in c1 == startCity || c2 == city0

