import Data.List (unfoldr)


-- Развернуть натуральное число n в список всех чисел, меньших его.
unfoldNatural = unfoldr (\n -> if n == 0 then Nothing else Just (n, n-1))


-- Развернуть число в список разрядов его двоичного представления.
unfoldBinary n = reverse $ unfoldr f n
    where
        f n = if n == 0 then Nothing else Just (b, a)
            where (a, b) = divMod n 2


-- Список разрядов преобразовать свёрткой в значение числа.
toDecHelper digits radix = foldl adder (0, 0, radix) (reverse digits)
    where
        adder (sum, pow, radix) digit = (sum + digit * (radix ^ pow), pow + 1,  radix)

getFirst (a, _, _) = a

toDec digits radix = getFirst $ toDecHelper digits radix


-- Развернуть число в список его простых делителей.
helper (number, factor)
    | number == 0 = Nothing
    | number == 1 = Just (number, (0, factor))
    | r == 0      = Just (factor, (q, factor))
    | otherwise   = helper (number, factor + 1)
    where
        (q, r) = divMod number factor

factorization n = unfoldr helper (n, 2)


-- Выразить список первых n чисел Фибоначчи через развёртку (модификация: бесконечный список)
fibb = unfoldr f (0, 1)
    where f (prev, curr) = Just (curr, (curr, prev + curr))

fibbN n = take n fibb


-- Развернуть число в сиракузскую последовательность.
syracuse = unfoldr f
    where
        f x
            | x == 0 = Nothing
            | x == 1 = Just (x, 0)
            | even x = Just (x, div x 2)
            | odd x  = Just (x, 3*x + 1)


-- Выразить список простых чисел, не превышающих n, через развёртку:
-- * с помощью решета Эратосфена
-- * модификация: бесконечный список всех простых чисел
primesHelper (x:xs) = Just (x, filter (\t -> mod t x /= 0) xs)

primes = unfoldr primesHelper (2:[3, 5 ..])