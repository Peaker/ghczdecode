-- Taken from GHC: compiler/utils/Encoding.hs

import Text.Encoding.Z (zDecodeString)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn . unwords $ map zDecodeString args
