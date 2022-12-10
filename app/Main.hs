import Prob9
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = parse inputParser "input.txt" contents
  case input of
    Left err -> putStrLn $ errorBundlePretty err
    Right value -> print $ solution' value