{- Given a number c, compute a and b such c = a * b.
 -
 - However, there is another constraint: given that c is 2n bits long, a and b
 - can have no more than n bits. Only c with even bitlength are handled, and a
 - and b, if they exist, will by necessity be exactly n bits long.
 -}
module SplitMaxLen where

import Prelude

import Control.Monad (forM_, when)
import Data.Bits (shiftR)
import Data.List (sort)
import Math.NumberTheory.Factor (pfactors)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

findSplit
  :: Integer
  -> Integer
  -> Maybe (Integer, Integer)
findSplit below = findSplit0 below 1 1 . sort . pfactors

findSplit0
  :: Integer
  -> Integer
  -> Integer
  -> [Integer]
  -> Maybe (Integer, Integer)
findSplit0 below l r [] = if (l < below && r < below) then
                            Just (l, r)
                          else
                            Nothing
findSplit0 below l r (u:us) =
  case findSplit0 below (l*u) r us of
    res@(Just _) -> res
    _ -> findSplit0 below l (r*u) us
{-
    _ -> -- If 'u' didn't fit, neither will other equal values
         let (allU, others) = span (== u) us
             r0 = r * u * product allU
         in findSplit0 below l r0 others
-}

showsBin
  :: Integer
  -> ShowS
showsBin b = go 0 b
 where
  go :: Int -> Integer -> ShowS
  go _ 0 = ("0b" ++)
  go 4 b0 = go 0 b0 . ('_':)
  go n b0 =   go (n+1) (b0 `shiftR` 1)
           . if even b0 then ('0':) else ('1':)

mainWith
  :: (Integer -> Integer -> Integer -> String)
  -> [String]
  -> IO ()
mainWith resFormatter args = do
  when (length args < 1 || length args > 2) $ do
    putStrLn "Invocation error."
    exitWith (ExitFailure 2)
  let prod = read (args!!0) :: Integer
      prodLen = ceiling $ log(fromInteger @Double prod) / log(2) :: Int
      below = 2 ^ (prodLen `div` 2) :: Integer
      incr = 2 ^ (read @Int (args!!1)) :: Integer
      prods = if length args < 2 then
                [prod]
              else
                [prod, prod + incr ..]
  when (odd prodLen) $ do
    putStrLn "A number with an even number of bits is required."
    putStrLn . ("Supplied number has " ++) . shows prodLen $ " bits."
    exitWith (ExitFailure 2)
  forM_ prods $ \p -> do
    putStrLn $ "Trying to find factors for\n  " ++ showsBin p ""
    case findSplit below p of
      Nothing -> pure ()
      Just (l, r) -> do
        putStrLn "Suitable split found:"
        putStrLn $ resFormatter p l r
        exitSuccess
  putStrLn "No suitable split found."
  exitWith (ExitFailure 1)

floatFormatter
  :: Integer
  -> Integer
  -> Integer
  -> String
floatFormatter p l r =
  let (pMant, pExp) = decodeFloat (fromInteger l * fromInteger r :: Float)
      pMantS = showsBin pMant
      pExpS = if pExp == 24 then ("digits" ++) else shows pExp
      pS = showsBin p
  in   ((     "    --"
         ++ "\n    -- ") ++) . pS
     . ((   "\n    -- " ++ replicate (length $ pS "") '-' ++ " round"
         ++ "\n    -- ") ++) . pMantS
     . ((   "\n    --"
         ++ "\n  , ( ") ++) . shows l
     . (    "\n    , " ++) . shows r
     . (    "\n    , encodeFloat " ++) . pMantS . (' ':) . pExpS
     $      "\n    )"

defaultFormatter
  :: Integer
  -> Integer
  -> Integer
  -> String
defaultFormatter p l r =
  ("    " ++) . shows p . (" = " ++) . shows l . (" * " ++) $ show r

defaultMain
  :: [String]
  -> IO ()
defaultMain = mainWith defaultFormatter
