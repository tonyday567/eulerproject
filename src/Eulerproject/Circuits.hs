-- | Euler solutions ported to Circuit.
module Eulerproject.Circuits
  ( -- * Fibonacci — lazy knot (Wire)
    euler2Wire,

    -- * Collatz — iteration (Step)
    euler14Step,

    -- * Collatz — ambient threading (Step + ambient)
    euler14Ambient,
  )
where

import Circuit (Circuit (..), Wire, ambient, reify)
import Data.Function ((&))
import Eulerproject (euler14Array')
import Eulerproject.Helpers (collatz)
import GHC.Arr qualified as Arr

-- ---------------------------------------------------------------------------
-- euler 2 — even Fibonacci (lazy knot)

-- | Sum of even Fibonacci numbers ≤ @limit@, using a lazy-knot Wire.
--
-- >>> euler2Wire 4000000
-- 4613732
euler2Wire :: Int -> Int
euler2Wire limit =
  let fibStream :: Wire () [Int]
      fibStream = Knot (Lift $ \(prev, ()) -> (1 : 2 : zipWith (+) prev (tail prev), prev))
      fibs = reify fibStream ()
   in takeWhile (<= limit) fibs
        & filter even
        & sum

-- ---------------------------------------------------------------------------
-- euler 14 — longest Collatz chain (Either iteration)

-- | Starting number under @n@ with longest Collatz chain, using Step iteration.
--
-- >>> euler14Step 1000000
-- 837799
euler14Step :: Int -> Int
euler14Step n =
  let body :: Either (Int, (Int, Int)) (Int, (Int, Int)) -> Either (Int, (Int, Int)) Int
      body (Left (cur, (maxLen, best))) =
        let len = collatz cur
            (maxLen', best') = if len > maxLen then (len, cur) else (maxLen, best)
         in if cur >= n
              then Right best'
              else Left (cur + 1, (maxLen', best'))
      body (Right seed) = body (Left seed)
   in reify (Knot (Lift body)) (2, (1, 1))

-- ---------------------------------------------------------------------------
-- euler 14 — ambient threading (Step + ambient)

-- | Starting number under @n@ with longest Collatz chain.
-- Precomputes collatz lengths into an array,
-- then uses @ambient@ to thread the array as read-only state
-- alongside the Step iteration.
--
-- >>> euler14Ambient 1000000
-- 837799
euler14Ambient :: Int -> Int
euler14Ambient n =
  let arr = euler14Array' n

      -- Step body — look up collatz lengths from the precomputed array.
      body :: Either (Int, (Int, Int)) (Int, (Int, Int)) -> Either (Int, (Int, Int)) Int
      body (Left (cur, (maxLen, best))) =
        let len = arr Arr.! cur
            (maxLen', best') = if len > maxLen then (len, cur) else (maxLen, best)
         in if cur >= n
              then Right best'
              else Left (cur + 1, (maxLen', best'))
      body (Right seed) = body (Left seed)

      -- Pin the ambient state type: array threads alongside the Step.
      circuit :: Circuit (->) Either (Either (Arr.Array Int Int) (Int, (Int, Int))) (Either (Arr.Array Int Int) Int)
      circuit = ambient (Knot (Lift body))
   in case reify circuit (Right (2, (1, 1))) of
        Right result -> result
        Left _ -> error "ambient: array state leaked"
