import Prelude

import System.Environment (getArgs)

import SplitMaxLen (mainWith, floatFormatter)

main :: IO ()
main = getArgs >>= mainWith floatFormatter
