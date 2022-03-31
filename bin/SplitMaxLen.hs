import Prelude

import System.Environment (getArgs)

import SplitMaxLen (defaultMain)

main :: IO ()
main = getArgs >>= defaultMain
