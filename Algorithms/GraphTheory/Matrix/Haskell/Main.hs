import System.Environment
import Control.Applicative
import Control.Monad

inputFile :: IO (FilePath)
inputFile = head <$> getArgs

outputFile :: IO (FilePath)
outputFile = flip (!!) 1 <$> getArgs

main :: IO ()
main = let content = readFile =<< inputFile
       in join $ writeFile <$> outputFile <*> content

