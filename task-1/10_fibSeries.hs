fibSeriesHelper :: Int -> Int -> [Int]
fibSeriesHelper a b = a : fibSeriesHelper b (a+b)

fibSeries = fibSeriesHelper 0 1