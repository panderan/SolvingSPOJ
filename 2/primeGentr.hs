main :: IO()
main = getLine >>= \n -> go (read n :: Int)
     where
         go i = case i of
             0 -> return ()
             _ -> getLine >>= \x -> (printRet $ sievePrime ([(n1 x)..(n2 x)]) 0 (n1 x) (all_prinums x)) >>= \_ -> go (i-1)
         n1  x = if (n1' x == 1) then 2 else n1' x                              -- Get n1 value, if n1 == 1, then let n1 equal to 2.
         n1' x = (fmap (\y -> read y :: Int) $ words x)!!0                      -- Just get n1 value
         n2  x = (fmap (\y -> read y :: Int) $ words x)!!1                      -- Just get n2 value
         max_dsr_num x = (fst . properFraction . sqrt . fromIntegral $ (n2 x)) + 1  -- The divisor nums of n2 can not be great than max_dsr_num_x
         all_prinums x = sievePrime [2..(max_dsr_num x)] 0 2 []                     -- All the prime nums less than max_dsr_num_x

-- Print result, one number a line         
printRet :: Show a => [a] -> IO()
printRet n = putStr $ concat $ fmap (\s -> show s ++ "\n") n

-- Sieve method 
-- @param ns            input nums list, including prime num and non-prime num
-- @param prosed        ns do not contain the nums which multiples of prime num less than prosed
-- @param n             n is first num of ns, but in the recursion, n is a num of ns
-- @param all_prinums   see main annotation
sievePrime :: [Int] -> Int -> Int -> [Int] -> [Int]
sievePrime ns prosed n all_prinums = 
    if (n == 0) then ns
    else if (prosed^2 > last ns) then ns
    else sievePrime nxt_ns nxt_siednum nxt_n all_prinums
    where
       sienums = getNxtSieNums n prosed max_sienum all_prinums
       nxt_ns = sieveOnce ns sienums
       nxt_siednum = if (sienums == []) then prosed else last sienums
       nxt_n = if (filter (>n) nxt_ns == []) then 0 else head $ filter (>n) nxt_ns
       max_sienum = (fst . properFraction . sqrt . fromIntegral . last $ ns) + 1

getNxtSieNums :: Int -> Int -> Int -> [Int] -> [Int]
getNxtSieNums n max_prosed max_sienum [] = if (isPrime (fromIntegral n) && n > max_prosed) then [n] else []
getNxtSieNums n max_prosed max_sienum all_n = [x | x <- all_n,  x <= max_sienum, x > max_prosed]

sieveOnce :: [Int] -> [Int] -> [Int]
sieveOnce [] _ = []
sieveOnce ns [] = ns
sieveOnce ns ps = sieveOnce [x | x <- ns, rem x n /= 0 || x == n] $ tail ps
    where
        n = head ps

--------------------------------------------------------------------------------
-- Following funcs copied from Data.Number.Primer 
--------------------------------------------------------------------------------
data Wheel = Wheel Integer [Integer] [Integer]
wheels = Wheel 1 [1] [] : zipWith3 nextSize wheels primes squares
squares = [p*p | p <- primes]

-- | Checks whether a number is prime    
isPrime :: Integer -> Bool
isPrime n      = all (not .(\p-> (n `mod` p) == 0)) $ takeWhile (\p -> p*p <= n) primes

primes :: [Integer] 
primes = spiral wheels primes squares

spiral (Wheel s ms ns : ws) ps qs = foldr (turn 0) (roll s) ns where
    roll o = foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
    turn o n rs = let n' = o+n in
        if n'==2 || n'< head qs then n':rs else dropWhile (<n') (spiral ws (tail ps) (tail qs))

nextSize :: Wheel -> Integer -> Integer -> Wheel
nextSize (Wheel s ms ns) p q = Wheel (s*p) ms' ns' where
    (xs, ns') = span (<=q) (foldr (turn 0) (roll (p-1) s) ns)
    ms' = foldr (turn 0) xs ms
    roll 0 _ = []
    roll t o = foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
    turn o n rs = let n' = o+n in [n' | n' `mod` p > 0] ++ rs
