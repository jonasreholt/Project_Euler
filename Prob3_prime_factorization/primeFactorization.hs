-- What is the largest prime factor of the number 600851475143

num = 600851475143

testNum = 13195 -- should give 5, 7, 13, 29


-- A faster approach utilizing that spf sends an item instead of a list (!lazy computing!)
-- as well as less function calls; 

-- |Finds largest prime factor of n.
lpf :: Integer -> Integer
lpf n
    | smlPFac == n  = smlPFac
    | otherwise     = lpf (n `div` smlPFac)
    where smlPFac = spf n

-- |Finds smallest prime factor of n.
spf :: Integer -> Integer
spf n = head [fac | fac <- [2..n], n `mod` fac == 0]