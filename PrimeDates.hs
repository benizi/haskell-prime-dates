module PrimeDates where

import Data.Maybe (catMaybes, isJust)
import Data.Time
import System.Environment (getArgs)

class Extractable a where
  asDay :: a -> Day
  asYMD :: a -> (Integer, Int, Int)
  asYMD = toGregorian . asDay
  getYear :: a -> Integer
  getYear = (\(y, _, _) -> y) . asYMD
  getMonth :: a -> Int
  getMonth = (\(_, m, _) -> m) . asYMD
  getDOM :: a -> Int
  getDOM = (\(_, _, d) -> d) . asYMD

instance Extractable Day where
  asDay = id

instance Extractable UTCTime where
  asDay = utctDay

daySeq :: Day -> [Day]
daySeq = iterate (addDays 1)

daysOfMonth :: Integer -> Int -> [Day]
daysOfMonth y m = takeWhile sameMonth $ daySeq (fromGregorian y m 1)
  where sameMonth = ((\(y',m',_) -> (y,m)==(y',m')) . asYMD)

daysOfYear :: Integer -> [Day]
daysOfYear y = takeWhile ((== y) . getYear) $ daySeq (fromGregorian y 1 1)

formatter :: String -> Day -> String
formatter = formatTime defaultTimeLocale

integerize :: String -> Day -> Integer
integerize fmt = read . formatter fmt

-- From: https://wiki.haskell.org/Fold
-- And: https://wiki.haskell.org/Prime_numbers#Initial_definition

pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t) = f x y : pairs f t
pairs _ t = t

foldi :: (a -> a -> a) -> a -> [a] -> a
foldi _ z [] = z
foldi f z (x:xs) = f x $ foldi f z $ pairs f xs

-- Assumes input lists are sorted
minus xx@(x:xs) yy@(y:ys) =
  case compare x y of
    LT -> x : minus xs yy
    EQ -> minus xs ys
    GT -> minus xx ys
minus xx _ = xx

-- Interesting bug: `union` from Data.List isn't right for this application
-- So, define it here.
union xx@(x:xs) yy@(y:ys) =
  case compare x y of
    LT -> x : union xs yy
    EQ -> x : union xs ys
    GT -> y : union xx ys
union xs [] = xs
union [] ys = ys

-- Fun sieve of Eratosthenes implementation
primes :: (Integral a) => [a]
primes = 2 : 3 : ([5,7..] `minus`
  foldi (\(x:xs) -> (x:) . union xs) []
    [[p*p,p*p+2*p..] | p <- tail primes])

-- naïve factoring implementation
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (not . dividesN) $ takeWhile ltSqrt candidates
  where dividesN = (== 0) . mod n
        ltSqrt = (<= n) . (^ 2)
        candidates = [2] ++ [3,5..]

ymdPrime = isPrime . integerize "%Y%m%d"
mdyPrime = isPrime . integerize "%m%d%Y"

primeBothWays d = ymdPrime d && mdyPrime d

class (Integral a, Read a) => MaybeRead a where
  parser :: ReadS a
  parser = readsPrec 0
  readMaybe :: String -> Maybe a
  readMaybe s =
    case parser s of
      [] -> Nothing
      [(val,"")] -> Just val
      _ -> Nothing

instance MaybeRead Integer
instance MaybeRead Int

debug :: (Show a) => [a] -> IO ()
debug = mapM_ (putStrLn . show)

main = do
  argv <- getArgs
  let argparses :: [Maybe Integer]
      argparses = readMaybe <$> argv
      parsed =
        if all isJust argparses
           then catMaybes argparses
           else []
  currentTime <- getCurrentTime
  let input =
        case parsed of
          [year] -> daysOfYear year
          [year, end] ->
            if end < year
               then daysOfMonth year (fromInteger end)
               else concatMap daysOfYear [year..end]
          _ -> daysOfYear $ getYear currentTime
  mapM_ (putStrLn . show) $ filter primeBothWays input
