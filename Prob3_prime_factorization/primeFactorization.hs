-- What is the largest prime factor of the number 600851475143

num = 600851475143

testNum = 13195 -- should give 5, 7, 13, 29


-- |A simple trial division method checks on numbers in [2..sqrt n].
isPrime :: Integer -> Bool
isPrime n = isPrime' n 2
    where
        isPrime' :: Integer -> Integer -> Bool
        isPrime' num tester
            | num == 1                                  = False
            | tester > (floor $ sqrt $ fromIntegral num)= True
            | num `mod` tester == 0                     = False
            | otherwise                                 = isPrime' num (tester + 1)

-- |Lists all prime number up till n.
primeList :: Integer -> [Integer]
primeList n =
    [i+1 | (x,i) <- zip [isPrime y | y <- [1..n]] [0..], x == True]


-- |Finds the largest prime factor of a number n.
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n =
    lpf n 0 (primeList $ floor $ sqrt $ fromIntegral n)
    where
        lpf :: Integer -> Integer -> [Integer] -> Integer
        lpf n largest primes
            | length primes <= 0            = if n > 1 then n else largest
            | n `mod` (head primes) == 0    = lpf (n `div` (head primes)) (head primes) (tail primes)
            | otherwise                     = lpf n largest (tail primes)

-- |Finds the largest prime factor of a number n looking from the top. This might not work
-- for some edge cases!
largestPrimeFactor' :: Integer -> Integer
largestPrimeFactor' n =
    if isPrime n then n else lpf n (primeList $ floor $ sqrt $ fromIntegral n)
    where
        lpf :: Integer -> [Integer] -> Integer
        lpf n primes
            | length primes <= 0            = 0
            | n `mod` (last primes) == 0    = last primes
            | otherwise                     = lpf n (init primes)