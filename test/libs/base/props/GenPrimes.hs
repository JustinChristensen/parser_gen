defFactor :: Double
defFactor = 1.618

defStart :: Int
defStart = 7

genprimes1 :: Double -> Int -> [Int] -> [Int]
genprimes1 factor p ps = genprimes' p ps
    where
        genprimes' _ [] = []
        genprimes' prime primes =
                let (_, (prime':primes')) = break (> candidate prime) primes
                in prime' : genprimes' prime' primes'
            where candidate p = ceiling (fromIntegral p * factor)


genprimes2 :: Double -> Int -> [Int] -> [Int]
genprimes2 factor n ps = genprimes' n ps
    where
        genprimes' _ [] = []
        genprimes' exp primes =
                let (_, (prime':primes')) = break (> candidate exp') primes
                in prime' : genprimes' exp' primes'
            where
                exp' = exp + 1
                candidate exp = ceiling (factor ^ exp)



