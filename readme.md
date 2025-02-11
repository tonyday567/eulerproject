eulerproject
===

[![img](https://img.shields.io/hackage/v/eulerproject.svg)](https://hackage.haskell.org/package/eulerproject)

**Spoiler Alert!**

Attempts at euler project solutions, using Haskell.

<https://projecteuler.net/>

    import Data.Char
    import Data.Functor.Rep
    import Data.List qualified as List
    import Data.Numbers.Primes (primes)
    import Data.Ord
    import Data.Proxy
    import GHC.Arr qualified as Arr
    import GHC.TypeNats
    import Harpie.Fixed qualified as F
    import NumHask.Prelude
    import Prelude qualified as P
    import Data.String.Interpolate
    import Data.Time.Calendar.WeekDate
    import Data.Time.Calendar
    -- import Data.Map.Strict qualified as Map
    import Data.Text qualified as Text
    import Control.Monad
    import Data.IntSet qualified as IntSet

    Build profile: -w ghc-9.8.1 -O1
    In order, the following will be built (use -v for more details):
     - eulerproject-0.0.1 (lib) (file src/Euler.hs changed)
    Preprocessing library for eulerproject-0.0.1..
    GHCi, version 9.8.1: https://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from /Users/tonyday567/haskell/eulerproject/.ghci
    [1 of 1] Compiling Euler            ( src/Euler.hs, interpreted )
    Ok, one module loaded.


# ToDo cleanups

-   [ ] euler 14: compare two solutions
-   [ ] euler 24 timing out
-   [ ] euler 25 timing out
-   [ ] euler 27 - clueless


# euler 32

[#32 Pandigital Products - Project Euler](https://projecteuler.net/problem=32)

    euler32

