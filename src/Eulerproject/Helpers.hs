module Eulerproject.Helpers
  ( -- * Prime utilities
    primeFactors,
    tau,
    largestPrimeFactor,
    sumDivs,

    -- * Combinatorial
    combinations,

    -- * Permutations
    permutations,

    -- * English number words (euler 17)
    letters,
    singles,
    teens,
    tens,

    -- * Collatz (euler 14)
    collatz,
    collatzStep,

    -- * Long division (euler 26)
    longDivStream,
  )
where

import Data.Bool (bool)
import Data.Function ((&))
import Data.List qualified as List
import Data.Numbers.Primes (primes)

-- ---------------------------------------------------------------------------
-- Prime utilities

-- | Prime factorization: descending list of prime factors.
-- Replaces the old @lfactor@, @pfactor@, @factors@, @factors'@, @pfactors'@.
--
-- >>> primeFactors 13195
-- [29,13,5]
primeFactors :: Int -> [Int]
primeFactors x = go x primes []
  where
    go _ [] xs = xs
    go n (p : ps) xs
      | p * p > n = n : xs
      | n `mod` p == 0 = go (n `div` p) (p : ps) (p : xs)
      | otherwise = go n ps xs

-- | Number of divisors.  @tau = product . map ((+1) . length) . List.group . primeFactors@.
--
-- >>> tau 28
-- 6
tau :: Int -> Int
tau = product . fmap ((+ 1) . length) . List.group . primeFactors

-- | Largest prime factor.
--
-- >>> largestPrimeFactor 13195
-- 29
largestPrimeFactor :: Int -> Int
largestPrimeFactor = head . primeFactors

-- | Sum of proper divisors (numbers less than n that divide n).
--
-- >>> sumDivs 220
-- 284
sumDivs :: Int -> Int
sumDivs x =
  ((`combinations` fs) <$> [1 .. length fs - 1])
    & mconcat
    & fmap product
    & List.nub
    & sum
    & (+ 1)
  where
    fs = primeFactors x

-- ---------------------------------------------------------------------------
-- Combinatorial

-- | k-combinations from a list.
--
-- >>> combinations 2 [0..4]
-- [[0,1],[0,2],[0,3],[0,4],[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations m l = [x : ys | x : xs <- List.tails l, ys <- combinations (m - 1) xs]

-- | Lexicographic permutations.
--
-- >>> permutations [0..2]
-- [[0,1,2],[0,2,1],[1,0,2],[1,2,0],[2,0,1],[2,1,0]]
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
  (ys, z : zs) <- zip (List.inits xs) (List.tails xs)
  (z :) <$> permutations (ys <> zs)

-- ---------------------------------------------------------------------------
-- English number words (euler 17)

singles, teens, tens :: [String]
singles = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- | English letters for a number < 1001.
--
-- >>> letters 342
-- "threehundredandfortytwo"
letters :: Int -> String
letters x
  | x < 10 = singles !! x
  | x < 20 = teens !! (x - 10)
  | x < 100 = case x `divMod` 10 of
      (x1, 0) -> tens !! (x1 - 2)
      (x1, x0) -> tens !! (x1 - 2) <> letters x0
  | x < 1000 = case x `divMod` 100 of
      (x2, 0) -> letters x2 <> "hundred"
      (x2, x0) -> letters x2 <> "hundred" <> "and" <> letters x0
  | x == 1000 = "one" <> "thousand"
  | otherwise = undefined

-- ---------------------------------------------------------------------------
-- Collatz (euler 14)

-- | Single Collatz step.
--
-- >>> collatzStep 13
-- 40
collatzStep :: Int -> Int
collatzStep x = bool (3 * x + 1) (x `div` 2) (even x)

-- | Collatz chain length.
--
-- >>> collatz 13
-- 10
collatz :: Int -> Int
collatz x0 = go x0 1
  where
    go n c =
      let n' = bool (3 * n + 1) (n `div` 2) (even n)
       in bool (go n' (c + 1)) (c + 1) (1 == n')

-- ---------------------------------------------------------------------------
-- Long division (euler 26)

-- | Stream of remainders from long division of 1/n.
longDivStream :: Int -> [Int]
longDivStream n =
  snd <$> List.unfoldr (\r -> Just (r `divMod` n, snd (r `divMod` n) * 10)) (10 ^ length (show n))
