import System.Environment
import Control.Applicative
import Control.Monad
import Data.List.Split
import qualified Data.Text as T
import Development.Placeholders
import Data.List as L
import Data.Int
import Data.Maybe

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

newtype RoadDestroyTime a = RoadDestroyTime Int64 deriving (Show, Eq, Ord)

minDestroyTime :: Int64
minDestroyTime = 1

maxDestroyTime :: Int64
maxDestroyTime = 1000000

toRoadDestroyTime :: Int64 -> RoadDestroyTime Int64
toRoadDestroyTime time
  | time < minDestroyTime = error $ "The destroy time for a road cannot be less than " ++ show minDestroyTime ++ "."
  | time > maxDestroyTime = error $ "The max destroy time for a road cannot be greater than " ++ show maxDestroyTime
                              ++ "."
  | otherwise = RoadDestroyTime time

fromRoadDestroyTime :: RoadDestroyTime Int64 -> Int64
fromRoadDestroyTime (RoadDestroyTime time) = time


data Road = Road { cities :: (City, City)
                 , destroyTime :: RoadDestroyTime Int64
                 } deriving (Show, Eq)

toRoad :: ((City, City), RoadDestroyTime Int64) -> Road
toRoad (cities, destroyTime) = Road cities destroyTime

fromRoad :: Road -> ((City, City), RoadDestroyTime Int64)
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

newtype KingdomTree a = KingdomTree [(City, [Road])] deriving Show

-- This creates a traversable tree of the Kingdom's city grid.
-- The data formate is an array of 2 element tuples with the
-- representations as [(City, roadsConnectedToCity)]
toKingdomTree :: Roads [Road] -> KingdomTree [(City, [Road])]
toKingdomTree (Roads roads) = KingdomTree $ buildTree 0 (-1) roads -- start with city 0 every time.
  where buildTree currentCity previousCity roads =
          let (connectedRoads, unconnectedRoads) = L.partition connectsToCurrentCity roads
          in if null connectedRoads then []
             else (currentCity, connectedRoads):(concat $ map (buildSubTree unconnectedRoads) connectedRoads)
          where otherCity (Road cities _) =
                  if currentCity == fst cities then snd cities else fst cities
                connectsToCurrentCity (Road cities _) =
                  currentCity == fst cities || currentCity == snd cities
                buildSubTree unconnectedRoads road =
                  let city = otherCity road
                  in if city == previousCity then []
                     else buildTree city currentCity $ road:unconnectedRoads

fromKingdomTree :: KingdomTree [(City, [Road])] -> [(City, [Road])]
fromKingdomTree (KingdomTree kingdom) = kingdom

-- Core logic

inputFile :: IO FilePath
inputFile = head <$> getArgs

outputFile :: IO FilePath
outputFile = flip (!!) 1 <$> getArgs

main :: IO ()
main = let content = readFile =<< inputFile
       in join $ writeFile <$> outputFile <*> (show . fourth . parseInput <$> content)
         where fourth (_, _, _, kingdom, _) = kingdom

splitLines :: T.Text -> [T.Text]
splitLines = filter (T.pack "" /=) . map T.strip . T.splitOn (T.pack "\n")

splitNumbers :: [T.Text] -> [[T.Text]]
splitNumbers = map $ T.splitOn $ T.pack " "

readNumbers :: [[T.Text]] -> [[Int]]
readNumbers = map $ map $ read . T.unpack

parseInput :: String -> (CityCount Int, MachineCount Int, Cities, KingdomTree [(City, [Road])], Machines [Machine Int])
parseInput st = let parsedInput = readNumbers . splitNumbers . splitLines . T.pack $ st
                    counts = head parsedInput
                    cityCount = toCityCount $ head counts
                    machineCount = toMachineCount (last counts) cityCount
                    cities = [0..fromCityCount cityCount - 1]
                    kingdom = toKingdomTree
                              $ toRoads cityCount
                              $ map toRoad
                              $ map (\triple -> ((head triple, triple!!1), toRoadDestroyTime $ fromIntegral $ triple!!2))
                                    $ tail $ take (fromCityCount cityCount) parsedInput
                    machines = toMachines machineCount
                               $ map (toMachine cityCount)
                                     $ sort $ flatten $ drop (fromCityCount cityCount) parsedInput
                in (cityCount, machineCount, cities, kingdom, machines)
                where flatten = map head

findPath :: City -> City -> [(City, [Road])] -> Maybe [Road]
findPath startCity endCity kingdom =
  let (activeNode, restOfKingdom) = findKingdomNode startCity kingdom
  in if (null restOfKingdom) || (null $ snd activeNode) then Nothing
     else if Nothing == (reachedEnd $ snd activeNode)
          then let path = catMaybes
                          $ map (\road -> findPath (otherCity road) endCity restOfKingdom)
                                $ snd activeNode
                   adjacentRoads = snd activeNode
                   linksToJoiningRoad adjacentRoad = let jointCity = otherCity adjacentRoad
                                                     in (jointCity == (fst . cities . head $ head path))
                                                        || (jointCity == (snd . cities . head $ head path))
                   newRoad = find linksToJoiningRoad adjacentRoads
               in case null $ head path of False -> case newRoad of Just road -> Just $ road:(head $ path)
                                                                    Nothing   -> Nothing
                                           True  -> Nothing
          else Just . maybeToList . reachedEnd $ snd activeNode
  where otherCity (Road cities _) =
          if startCity == fst cities then snd cities else fst cities
        reachedEnd roads = listToMaybe $ catMaybes
                           $ map (\(Road cities time) ->
                                     if endCity == fst cities || endCity == snd cities
                                     then Just (toRoad (cities, time))
                                     else Nothing)
                                 roads

-- The input validations should make it nearly impossible for this function
-- to be provided with a city that does not exist in the provided kingdom
-- partial.
findKingdomNode :: City -> [(City, [Road])] -> ((City, [Road]), [(City, [Road])])
findKingdomNode topCity partialKingdom =
  let (beforeMatched, afterMatched) = L.break matchesTopCity partialKingdom
  in if null afterMatched then (((-1), []), partialKingdom)
     else ((head afterMatched), beforeMatched ++ tail afterMatched)
  where matchesTopCity node = topCity == fst node

