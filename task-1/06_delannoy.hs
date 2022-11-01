delannoy a 0 = 1
delannoy 0 b = 1
delannoy a b =
  let
    a' = a - 1
    b' = b - 1
  in delannoy a' b + delannoy a b' + delannoy a' b'

--

delannoyStep cs ds = 1 : (zipWith3 (\x y z -> x + y + z) ds (tail ds) cs) ++ [1]

delannoyHelper cs ds = cs : delannoyHelper ds (delannoyStep cs ds)

delannoyLayers = delannoyHelper [1] [1, 1]