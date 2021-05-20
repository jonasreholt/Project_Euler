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
        revInt acc x = revInt (acc * 10 + (x `mod` 10)) (x `div` 10)

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
    let x = replicate9 n in
       foldl1 max (lpf x x)
    where
        lpf x y
            | x == lowb && y /= lowb      = lpf (y-1) (y-1)
            | x == lowb && y == lowb      = []
            | isPalindromic prod    = prod : lpf (x-1) y
            | otherwise             = lpf (x-1) y
            where
                prod = x*y
                lowb = 10 ^ (n-1)