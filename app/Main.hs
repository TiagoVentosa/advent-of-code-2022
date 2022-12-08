import System.IO
import Prob6
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = parse inputParser "input.txt" contents
  case input of
    Left err -> putStrLn $ errorBundlePretty err
    Right value -> print $ solution' value