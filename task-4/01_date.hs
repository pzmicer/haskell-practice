{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (isDigit)
-- import GHC.Read (Read(readPrec, readListPrec), readListPrecDefault)


data Date = Date Int Int Int

instance Eq Date where
  Date d1 m1 y1 == Date d2 m2 y2 = d1 == d2 && m1 == m2 && y1 == y2

instance Ord Date where
  Date d1 m1 y1 <= Date d2 m2 y2 =
    y1 <= y2 && m2 <= m2 && d1 <= d2

instance Show Date where
  show (Date d m y) =
    (if d < 10 then "0" else "")
      ++ show d
      ++ "."
      ++ (if m < 10 then "0" else "")
      ++ show m
      ++ "."
      ++ show y

instance Read Date where
  readsPrec _ (d1 : d2 : '.' : m1 : m2 : '.' : rest) =
    let d = read [d1, d2] :: Int
        m = read [m1, m2] :: Int
        (y_str, rest) = span isDigit rest
        y = read y_str :: Int
     in [(Date d m y, rest) | all isDigit ([d1, d2, m1, m2] ++ y_str)]

isLeap y
  | y `mod` 4 /= 0 = False
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | otherwise = True

getDaysInMonth m y = case m of
  1 -> 31
  2 -> if isLeap y then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31

getDaysInYear y = if isLeap y then 366 else 365

newDate d m y
  | isValidDate d m y = Date m d y
  | otherwise = error "invalid Date"

isValidDate d m y
  | y < 0 = False
  | m < 1 || m > 12 = False
  | d < 1 || d > getDaysInMonth m y = False
  | otherwise = True

toDays (Date d m y) = go (m-1) y 0
  where
    go m y s
      | y > 0 = go m (y - 1) (s + getDaysInYear y)
      | m > 0 = go (m - 1) y (s + getDaysInMonth m y)
      | otherwise = s + d

toDate n = go n 1 0
  where
    go n m y
      | n >= getDaysInYear y = go (n - getDaysInYear y) m (y + 1)
      | n > getDaysInMonth m y = go (n - getDaysInMonth m y) (m + 1) y
      | otherwise = newDate n m y

add :: Date -> Int -> Date
add date n = toDate (toDays date + n)

substract :: Date -> Date -> Date
substract d1 d2 = go (toDays d1 - toDays d2)
  where
    go ds = if ds < 0 then newDate 1 1 0 else toDate ds