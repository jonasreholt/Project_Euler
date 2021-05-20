-- Find the largest palindrome made from the product of two 3-digit numbers.

-- 2-digit numbers should give 9009 = 91 * 99
-- 3-digit should be 906609.

{-
We brute force this problem. So for all number pairs between 100 and 999
we check if it is a palindrome. If it is we add it to a result list; at the end
we find the max value in that list.

Note the following:
    * let the number pair be n1 and n2, then we only check numbers where n1 <= n2
      to avoid redundant checks.
    * We cannot return the first palindrome we find, as we keep n2 high, and lower
      n1 until it hits lower bound. Meaning we will not find 906609, for the 3-digit
      test, as it will find a palindrom while n2=999 and n1 is descending. Hence
      we list all found palindromes, and return max.
-}

reverseInt :: Int -> Int
reverseInt n =
    revInt 0 n
    where
        revInt acc 0 = acc
        revInt acc rmd = revInt (acc * 10 + (rmd `mod` 10)) (rmd `div` 10)

isPalindromic :: Int -> Bool
isPalindromic n =
    n == reverseInt n

replicate9 :: Int -> Int
replicate9 n
    | n <= 0 = 0
    | n == 1 = 9
    | otherwise = concatInt' (replicate9 (n-1)) 9
    where
        concatInt' a b
            | a < 0     = a * 10 - b
            | otherwise = a * 10 + b

largestPalindromic :: Int -> Int
largestPalindromic n =
    maximum [a * b | a <- [low .. high], b <- [low .. high], isPalindromic (a*b)]
    where
        low = 10^(n-1)
        high = replicate9 n