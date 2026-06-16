{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Eulerproject
  ( -- * solutions
    euler1,
    euler2,
    euler3,
    euler4,
    euler5,
    euler6,
    euler7,
    euler8,
    euler9,
    euler10,
    euler11,
    euler12,
    euler13,
    euler14Array,
    euler14Array',
    euler14,
    euler15,
    euler16,
    euler17,
    euler18,
    euler19,
    euler20,
    euler21,
    euler22,
    euler23,
    euler24,
    euler25,
    euler26,
    euler27,
    euler28,
    euler29,
    euler30,
    euler31,

    -- * collatz variants (harpie)
    collatzArray,
    collatz2,
  )
where

import Control.Category ((>>>))
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Char (digitToInt, ord)
import Data.Foldable (maximumBy)
import Data.Function ((&))
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Numbers.Primes (primes)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Time.Calendar (DayOfWeek (..), dayOfWeek, fromGregorian)
import Data.Traversable (mapAccumL)
import Eulerproject.Data (coinCombinations, coins, digits, euler11Data, euler13Data, tri18)
import Eulerproject.Helpers (collatz, collatzStep, largestPrimeFactor, letters, longDivStream, permutations, sumDivs, tau)
import GHC.Arr qualified as Arr
import GHC.TypeNats (KnownNat, natVal)
import Harpie.Fixed qualified as F
import Prelude as P hiding (cycle)

-- $setup
--
-- >>> import Eulerproject

-- ---------------------------------------------------------------------------
-- euler 1 — sum of multiples

-- | Sum of multiples of @ns@ below @limit@.
--
-- >>> euler1 [3,5] 1000
-- 233168
euler1 :: [Int] -> Int -> Int
euler1 ns limit =
  [0 ..]
    & take limit
    & filter (\x -> any (\n -> x `mod` n == 0) ns)
    & sum

-- ---------------------------------------------------------------------------
-- euler 2 — even Fibonacci

-- | Sum of even Fibonacci numbers not exceeding @limit@, seeded with @(1,2)@.
--
-- >>> euler2 4000000 (1,2)
-- 4613732
euler2 :: Int -> (Int, Int) -> Int
euler2 limit (s0, s1) =
  [s0, s1]
    <> List.unfoldr
      ( \(s0', s1') ->
          let n = s0' + s1'
           in Just (n, (s1', n))
      )
      (s0, s1)
    & takeWhile (<= limit)
    & filter even
    & sum

-- ---------------------------------------------------------------------------
-- euler 3 — largest prime factor

-- | Largest prime factor of @n@.
--
-- >>> euler3 600851475143
-- 6857
euler3 :: Int -> Int
euler3 = largestPrimeFactor

-- ---------------------------------------------------------------------------
-- euler 4 — largest palindrome product

-- | Largest palindrome made from product of two numbers in range.
--
-- >>> euler4 (100, 999)
-- 906609
euler4 :: (Int, Int) -> Int
euler4 (lo, hi) =
  (*)
    <$> [lo .. hi]
    <*> [lo .. hi]
    & filter (\x -> let s = show x in s == reverse s)
    & maximum

-- ---------------------------------------------------------------------------
-- euler 5 — smallest multiple

-- | Smallest positive number evenly divisible by all numbers 1..@n@.
--
-- >>> euler5 20
-- 232792560
euler5 :: Int -> Int
euler5 n = foldl' (\x a -> (a * x) `div` gcd a x) 1 [1 .. n]

-- ---------------------------------------------------------------------------
-- euler 6 — sum square difference

-- | Difference between sum-of-squares and square-of-sum for 1..@n@.
--
-- >>> euler6 100
-- 25164150
euler6 :: Int -> Int
euler6 n =
  [1 .. n]
    & (\x -> (sum x * sum x) - sum ((^ 2) <$> x))

-- ---------------------------------------------------------------------------
-- euler 7 — 10,001st prime

-- | The @n@th prime.
--
-- >>> euler7 10001
-- 104743
euler7 :: Int -> Int
euler7 n = primes List.!! (n - 1)

-- ---------------------------------------------------------------------------
-- euler 8 — largest product in series

-- | Largest product of @n@ adjacent digits.
--
-- >>> euler8 13
-- 23514624000
euler8 :: Int -> Int
euler8 n =
  (\i -> product $ take n $ drop i (subtract 48 . ord <$> digits))
    <$> [0 .. length digits - n]
    & maximum

-- ---------------------------------------------------------------------------
-- euler 9 — Pythagorean triplet

-- | Product of Pythagorean triplet summing to @n@.
--
-- >>> euler9 1000
-- [31875000]
euler9 :: Int -> [Int]
euler9 n =
  filter
    (\(a, b, c) -> a <= b && a + b + c == fromIntegral n)
    ( (\a b -> (a, b, sqrt (a * a + b * b)))
        <$> fmap fromIntegral [1 .. n `div` 2]
        <*> fmap fromIntegral [1 .. n `div` 2]
    )
    & fmap (\(a, b, c) -> floor a * floor b * (floor c :: Int))

-- ---------------------------------------------------------------------------
-- euler 10 — sum of primes

-- | Sum of primes below @n@.
--
-- >>> euler10 2000000
-- 142913828922
euler10 :: Int -> Int
euler10 n = sum $ takeWhile (< n) primes

-- ---------------------------------------------------------------------------
-- euler 11 — largest grid product

-- | Largest product of 4 adjacent numbers (horizontal, vertical, diagonal, reverse-diagonal).
--
-- >>> euler11
-- 70600674
euler11 :: Int
euler11 = maximum (h <> v <> d <> r)
  where
    xs = maybe (-1) fst . listToMaybe . reads <$> words euler11Data :: [Int]
    a = F.array @[20, 20] @Int xs
    h = (\i j -> product ((\x -> F.unsafeIndex a [i, j + x]) <$> [0 .. 3])) <$> [0 .. 19] <*> [0 .. 16]
    v = (\i j -> product ((\x -> F.unsafeIndex a [i + x, j]) <$> [0 .. 3])) <$> [0 .. 16] <*> [0 .. 19]
    d = (\i j -> product ((\x -> F.unsafeIndex a [i + x, j + x]) <$> [0 .. 3])) <$> [0 .. 16] <*> [0 .. 16]
    r = (\i j -> product ((\x -> F.unsafeIndex a [i + x, j - x]) <$> [0 .. 3])) <$> [0 .. 16] <*> [3 .. 19]

-- ---------------------------------------------------------------------------
-- euler 12 — highly divisible triangular number

-- | First triangular number with more than @x@ divisors.
--
-- >>> euler12 500
-- 76576500
euler12 :: Int -> Int
euler12 x = head $ filter ((> x) . tau) $ List.scanl1 (+) [1 ..]

-- ---------------------------------------------------------------------------
-- euler 13 — large sum

-- | First 10 digits of sum of 100 50-digit numbers.
--
-- >>> euler13
-- "5537376230"
euler13 :: String
euler13 = take 10 $ show $ sum (fmap (maybe (-1) fst . listToMaybe . reads) (lines euler13Data) :: [Integer])

-- ---------------------------------------------------------------------------
-- euler 14 — longest Collatz chain

-- | Starting number under @n@ with longest Collatz chain (array-backed).
--
-- >>> euler14Array 1000000
-- 837799
euler14Array :: Int -> Int
euler14Array n = fst $ maximumBy (comparing snd) $ Arr.assocs a
  where
    a = Arr.listArray (1, n) $ (0 :: Int) : map syr [2 .. n]
    syr x =
      if y <= n then 1 + a Arr.! y else 1 + syr y
      where
        y = collatzStep x

euler14Array' :: Int -> Arr.Array Int Int
euler14Array' n = a
  where
    a = Arr.listArray (1, n) $ (0 :: Int) : map syr [2 .. n]
    syr x =
      if y <= n then 1 + a Arr.! y else 1 + syr y
      where
        y = collatzStep x

collatzArray :: F.Array '[10000] Int
collatzArray =
  F.unsafeTabulate go
  where
    go [] = error "empty shape"
    go (0 : _) = 1
    go (i : _) = collatz2 collatzArray i

collatz2 ::
  forall n. (KnownNat n) => F.Array '[n] Int -> Int -> Int
collatz2 arr x =
  let y = collatzStep (x + 1)
   in bool (1 + collatz2 arr y) (1 + F.unsafeIndex arr [y]) (y <= s)
  where
    s = fromIntegral $ natVal @n Proxy

-- | Starting number under @n@ with longest Collatz chain (recursive).
euler14 :: Int -> Int
euler14 n = go 1 (1, 1)
  where
    go x (max', maxx) =
      bool
        maxx
        (go (x + 1) (bool (max', maxx) (collatz x, x) (collatz x > max')))
        (n >= x)

-- ---------------------------------------------------------------------------
-- euler 15 — lattice paths

-- | Number of routes through an @n×n@ grid (central binomial coefficient).
--
-- >>> euler15 20
-- 137846528820
euler15 :: Int -> Int
euler15 n = iterate (List.scanl1 (+)) (repeat 1) List.!! n List.!! n

-- ---------------------------------------------------------------------------
-- euler 16 — power digit sum

-- | Sum of digits of @2^x@.
--
-- >>> euler16 1000
-- 1366
euler16 :: Int -> Int
euler16 x = show (2 ^ x :: Integer) & fmap digitToInt & sum

-- ---------------------------------------------------------------------------
-- euler 17 — number letter counts

-- | Sum of letter counts for numbers in range.
--
-- >>> euler17 [1..1000]
-- 21124
euler17 :: [Int] -> Int
euler17 xs = sum $ length . letters <$> xs

-- ---------------------------------------------------------------------------
-- euler 18 — maximum path sum

-- | Brute force: maximum path sum through triangle @tri18@.
--
-- >>> euler18 14
-- 1074
euler18 :: Int -> Int
euler18 level =
  foldr (\x y -> (:) <$> x <*> y) [[0]] (replicate level [0, 1])
    & fmap
      ( reverse
          >>> accsum
          >>> zip [0 ..]
          >>> fmap (\(x1, x2) -> (tri18 !! x1) !! x2)
          >>> sum
      )
    & maximum
  where
    accsum = P.snd . mapAccumL (\a b -> (a + b, a + b)) 0

-- ---------------------------------------------------------------------------
-- euler 19 — counting Sundays

-- | Number of Sundays on the 1st of a month in the 20th century.
--
-- >>> euler19
-- 171
euler19 :: Int
euler19 =
  fromGregorian <$> [1901 .. 2000] <*> [1 .. 12] <*> [1]
    & fmap dayOfWeek
    & filter (== Sunday)
    & length

-- ---------------------------------------------------------------------------
-- euler 20 — factorial digit sum

-- | Sum of digits of @x!@.
--
-- >>> euler20 100
-- 648
euler20 :: Integer -> Int
euler20 x = product [1 .. x] & show & fmap digitToInt & sum

-- ---------------------------------------------------------------------------
-- euler 21 — amicable numbers

-- | Sum of amicable numbers under @n@.
--
-- >>> euler21 10000
-- 31626
euler21 :: Int -> Int
euler21 n =
  [1 .. n]
    & filter (\x -> let s = sumDivs x in s /= x && sumDivs s == x)
    & sum

-- ---------------------------------------------------------------------------
-- euler 22 — name scores

-- | Total name scores for sorted names from file.
--
-- >>> euler22
-- 871198282
euler22 :: IO Int
euler22 = do
  str <- readFile "other/0022_names.txt"
  let names =
        List.sort $
          fromMaybe undefined
            . ( Text.stripSuffix "\""
                  <=< Text.stripPrefix "\""
              )
            <$> Text.splitOn "," (Text.pack str)
  pure $ sum $ zipWith (*) [1 ..] $ fmap (sum . fmap (\c -> ord c - ord 'A' + 1) . Text.unpack) names

-- ---------------------------------------------------------------------------
-- euler 23 — non-abundant sums

-- | Sum of positive integers not expressible as sum of two abundant numbers.
--
-- >>> euler23 28123
-- 4179871
euler23 :: Int -> Int
euler23 n =
  ((n * (n + 1)) `div` 2) - sum (filter (<= n) $ IntSet.toList abundantSet)
  where
    abundants = filter (\x -> x < sumDivs x) [1 .. n]
    abundantSet = ((+) <$> abundants <*> abundants) & IntSet.fromList

-- ---------------------------------------------------------------------------
-- euler 24 — millionth permutation

-- | The @i@th lexicographic permutation of @[0..x-1]@.
--
-- >>> euler24 1000000 10
-- "2783915460"
euler24 :: Int -> Int -> String
euler24 i x = permutations [0 .. x - 1] List.!! (i - 1) & fmap show & mconcat

-- ---------------------------------------------------------------------------
-- euler 25 — 1000-digit Fibonacci index

-- | Index of first Fibonacci number with @x@ digits.
--
-- >>> euler25 1000
-- 4782
euler25 :: Int -> Int
euler25 x =
  maybe undefined (1 +) $
    List.elemIndex
      x
      (length . show <$> List.unfoldr (\(x0, x1) -> Just (x1, (x1, x0 + x1))) (0 :: Integer, 1))

-- ---------------------------------------------------------------------------
-- euler 26 — longest recurring cycle

-- | Denominator under @n@ with longest recurring decimal cycle.
--
-- >>> euler26 1000
-- 983
euler26 :: Int -> Int
euler26 n = 1 + fromMaybe undefined (List.elemIndex True $ fmap (\x -> maximum xs' == x) xs')
  where
    xs' = rep <$> [1 .. n - 1]
    rep x = go [] $ longDivStream x
    go [] [] = 0
    go _ (0 : _) = 0
    go res (x : xs) = maybe (go (x : res) xs) (+ 1) (x `List.elemIndex` res)
    go _ [] = 0

-- ---------------------------------------------------------------------------
-- euler 27 — quadratic primes

-- | Quadratic formula for Euler's prime-generating polynomial.
euler27 :: Int -> Int -> Int -> Int
euler27 a b n = n * n + a * n + b

-- ---------------------------------------------------------------------------
-- euler 28 — spiral diagonal sum

-- | Sum of diagonals of a 1001×1001 spiral.
--
-- >>> euler28
-- 669171001
euler28 :: Int
euler28 = 1 + sum ((\x -> 4 * x * x - 6 * (x - 1)) <$> [3, 5 .. 1001])

-- ---------------------------------------------------------------------------
-- euler 29 — distinct powers

-- | Count of distinct terms in @a^b@ for @2 ≤ a,b ≤ 100@.
--
-- >>> euler29
-- 9183
euler29 :: Int
euler29 = length $ List.nub $ (^) <$> [2 .. 100 :: Integer] <*> [2 .. 100 :: Integer]

-- ---------------------------------------------------------------------------
-- euler 30 — digit fifth powers

-- | Sum of numbers equal to sum of fifth powers of their digits.
--
-- >>> euler30
-- 443839
euler30 :: Int
euler30 =
  [2 .. 5 * 9 ^ (5 :: Int)]
    & filter
      ( \x ->
          show x
            & fmap (digitToInt >>> (^ (5 :: Int)))
            & sum
            & (== x)
      )
    & sum

-- ---------------------------------------------------------------------------
-- euler 31 — coin sums

-- | Number of ways to make £2 using any combination of coins.
--
-- >>> euler31
-- 73682
euler31 :: Int
euler31 = length $ coinCombinations coins !! 200
