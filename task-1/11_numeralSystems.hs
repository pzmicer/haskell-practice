-- fromDigits [1, 0, 1, 1, 0, 1] 2 == 45
fromDigitsHelper [] n p res = res
fromDigitsHelper (d:ds) n p res = fromDigitsHelper ds n (p * n) (res + d * p)
fromDigits ds n = fromDigitsHelper (reverse ds) n 1 0

-- toDigits 45 2 == [1, 0, 1, 1, 0, 1]
toDigitsR 0 _ = [0]
toDigitsR x n =
    let (y, z) = divMod x n
    in z : toDigits y n

toDigits x n = reverse $ toDigitsR x n

-- addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1] = [1, 1, 0, 1, 0, 0]
addHelper [] [] _ _ = []
addHelper a [] n rem = addHelper a [0] n rem
addHelper [] b n rem = addHelper [0] b n rem
addHelper (a:as) (b:bs) n rem =
    let (y, z) = divMod (a + b + rem) n
    in z : addHelper as bs n y

add a b n = reverse $ addHelper (reverse a) (reverse b) n 0