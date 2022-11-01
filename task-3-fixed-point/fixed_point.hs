import Data.Function


-- 1. GCD
hGCD :: (Int -> Int -> Int) -> Int -> Int -> Int
hGCD f a b = case compare a b of GT -> f (a - b) b
                                 LT -> f a (b - a)
                                 EQ -> a

testGCD = fix hGCD 5 10


-- 2. fibSeries
hFibSeries f a b = a : f b (a+b)

testFibSeries = take 10 $ fix hFibSeries 0 1


-- 3. fibByNumber
hFibByNumber f a b n = if n == 0 then a else f b (a+b) (n-1)

testFibByNumber = fix hFibByNumber 0 1 7


-- 4. listSum
hListSum :: ([Int] -> Int) -> [Int] -> Int
hListSum f l = if null l then 0 else head l + f (tail l)

testListSum = fix hListSum [1, 2, 3]


-- 5. minList
hMinList f [] min = min
hMinList f (x:xs) min = if x < min then f xs x else f xs min

minList [] = 0
minList l = fix hMinList l (head l)

testMinList = minList [1, 2, 3]


-- 6. reverseList
hReverse f [] l = l
hReverse f (x:xs) l = f xs (x:l)

reverseList l = fix hReverse l []

testReverseList = reverseList [1, 2, 3]