{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Euler where

import NumHask.Prelude
import Data.Numbers.Primes (primes)
import qualified Data.List as List
import qualified NumHask.Array.Fixed as A
-- import NumHask.Array.Shape
import Text.InterpolatedString.Perl6
import Data.Functor.Rep
import qualified Prelude as P
import Data.Char
import qualified GHC.Arr as Arr
import Data.Ord
import GHC.TypeNats
import Data.Proxy

-- | Sum of Multiples
--
-- >>> euler1 [3,5] 1000
-- 233168
euler1 :: [Int] -> Int -> Int
euler1 ns limit =
  [0..] &
  take limit &
  filter (\x -> any (\n -> x `mod` n == 0) ns) &
  sum

-- | even fibonacci
--
-- classic: fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
--
-- >>> euler2 4000000 (1,2)
-- 4613732
--
euler2 :: Int -> (Int, Int) -> Int
euler2 limit s@(s0,s1) =
  [s0,s1] <>
  List.unfoldr (\(s0',s1') ->
             let n = s0' + s1' in
               Just (n, (s1',n))) s &
  takeWhile (<= limit) &
  filter ((==0) . (`mod` 2)) &
  sum

-- | primes
--
-- >>> euler3 600851475143
-- 6857
euler3 :: Int -> Int
euler3 n = lfactor n primes
  where
    lfactor _ [] = undefined
    lfactor n (p:ps)
      | p*p > n = n
      | n `mod` p == 0 = lfactor (n `div` p) (p:ps)
      | otherwise = lfactor n ps

-- | palindrome product
--
-- >>> euler4 (100, 999)
-- 906609
euler4 :: (Int,Int) -> Int
euler4 (x,x') =
  maximum $
  fst <$>
  filter ((\x -> x == reverse x) . snd) (
   (\x -> (x, show x)) <$>
   ((*) <$>
    [x..x'] <*>
    [x..x']))

-- | euler5
--
-- >>> euler5 20
-- 232792560
--
euler5 :: Int -> Int
euler5 n = foldl' (\x a -> (a * x) `div` gcd a x) 1 [1..n]

-- | euler6
--
-- >>> euler6 100
-- 25164150
euler6 :: Int -> Int
euler6 n = [1..n] & (\x -> (sum x * sum x) - sum ((\x -> x*x) <$> x))

-- | primes
--
-- >>> euler7 10001
-- 104743
euler7 :: Int -> Int
euler7 n = primes List.!! (n - 1)

-- | euler8
--
-- >>> euler8 13
-- 23514624000
euler8 :: Int -> Int
euler8 n =
  maximum $
  (\i ->
     product $
     take n $
     drop i $ (\x -> x - 48) <$>
     (ord <$> digits)) <$>
  [0.. (length digits - n)]

-- | euler9
--
-- >>> euler9 1000
-- [31875000]
euler9 :: Int -> [Int]
euler9 n =
  (\(a,b,c) -> floor a * floor b * (floor c :: Int)) <$>
  filter (\(a,b,c) -> (a<=b) && a+b+c == fromIntegral n) (
   (\a b -> (a,b,sqrt (a*a + b*b))) <$>
   [1..(fromIntegral (n `div` 2) :: Double)] <*>
   [1..(fromIntegral (n `div` 2) :: Double)])

-- | euler10
--
-- >>> euler10 2000000
-- 142913828922
euler10 :: Int -> Int
euler10 n = sum $ takeWhile (<n) primes

digits :: String
digits = filter (not . (`elem` [' ','\n','\r'])) [q|
    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450
|]

euler11Data :: String
euler11Data = [q|
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
|]

-- | euler11
--
-- >>> euler11
-- 70600674
euler11 :: Int
euler11 = maximum (h <> v <> d <> r)
  where
    xs = maybe (-1) fst . listToMaybe . reads <$> words euler11Data :: [Int]
    a = fromList xs :: A.Array '[20,20] Int
    h = (\i j -> product ((\x -> index a [i,j+x]) <$> [0..3])) <$> [0..19] <*> [0..16]
    v = (\i j -> product ((\x -> index a [i+x,j]) <$> [0..3])) <$> [0..16] <*> [0..19]
    d = (\i j -> product ((\x -> index a [i+x,j+x]) <$> [0..3])) <$> [0..16] <*> [0..16]
    r = (\i j -> product ((\x -> index a [i+x,j-x]) <$> [0..3])) <$> [0..16] <*> [3..19]

factors :: Int -> [Int]
factors x = go x 1 []
  where
    go x n fs
      | x < n * n = fs
      | x `mod` n == 0 = go x (n+1) (bool (n:(x `div` n):fs) (n:fs) (n == x `div` n))
      | otherwise = go x (n+1) fs

factors' x = product $ (+1) . length <$> List.group (pfactor x primes)

pfactor :: (Ord a, Integral a, FromInteger a) => a -> [a] -> [a]
pfactor n (p:ps)
  | p*p > n = [n]
  | n `mod` p == 0 = p : pfactor (n `div` p) (p:ps)
  | otherwise = pfactor n ps

-- | euler12
--
-- >>> euler12 500
-- 76576500
euler12 x = head $ filter ((>x) . factors') $ List.scanl1 (+) [1..]

euler13Data :: String
euler13Data = [q|
37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690
|]


-- | euler13
--
-- >>> euler13
-- "5537376230"
euler13 :: String
euler13 = take 10 $ show $ sum (maybe (-1) fst . listToMaybe . reads <$> lines euler13Data :: [Integer])

-- * collatz experiments

-- | euler14 array version
--
-- >>> euler14Array 1000000
-- 837799
--
--
euler14Array :: Int -> Int
euler14Array n = fst $ maximumBy (comparing snd) $ Arr.assocs a
  where
    a = Arr.listArray (1,n) $ (0::Int) : map syr [2..n]
    syr x =
        if y <= n then 1 + a Arr.! y else 1 + syr y
        where
        y = if even x then x `div` 2 else 3 * x + 1

euler14Array' :: Int -> Arr.Array Int Int
euler14Array' n = a
  where
    a = Arr.listArray (1,n) $ (0::Int) : map syr [2..n]
    syr x =
        if y <= n then 1 + a Arr.! y else 1 + syr y
        where
        y = if even x then x `div` 2 else 3 * x + 1

collatzArray :: A.Array '[10000] Int
collatzArray =
  tabulate go
  where
    go (0:_) = 1
    go (i:_) = collatz2 collatzArray i

collatz1 :: Int -> Int
collatz1 x = bool (x `div` 2) (3*x+1) (even x)

collatz2 ::
  forall n. (KnownNat n) => A.Array '[n] Int -> Int -> Int
collatz2 arr x = let y = collatz1 (x+1) in
  bool (1+ collatz2 arr y) (1 + index arr [y]) (y <= s)
  where
    s = P.fromIntegral $ natVal @n Proxy

collatz :: Int -> Int
collatz x = go x 1
  where
    go x n = let x' = bool (3*x+1) (x `div` 2) (even x) in
      bool (go x' (n+1)) (n+1) (1==x')

-- | euler14
euler14 :: Int -> Int
euler14 n = go 1 (1,1)
  where
    go x (max', maxx) =
      bool
      maxx
      (go (x+1) (bool (max', maxx) (collatz x, x) (collatz x > max')))
      (n >= x)

euler15 :: Int -> Int
euler15 n = iterate (List.scanl1 (+)) (repeat 1) List.!! n List.!! n

euler16 :: Int -> Int
euler16 x = sum $ (\x -> ord x - 48) <$> (show (2 P.^ x :: Integer))

main :: IO ()
main =
  print ()