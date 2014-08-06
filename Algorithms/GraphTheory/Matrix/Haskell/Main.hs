import System.Environment

main :: IO ()
main =do
  cliArgs <- getArgs
  let inputFile = head cliArgs
      outputFile = cliArgs!!1
  contents <- readFile inputFile
  writeFile outputFile contents

