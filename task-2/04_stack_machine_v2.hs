type Stack a = [a]
data Command a
    = StUnOp  (a -> a)
    | StBinOp (a -> a -> a)
    | StSpecial Int ([a] -> [a])

type Program a = [Command a]

doCommand :: Command a -> Stack a -> Stack a
doCommand (StUnOp f) (x : ts) = f x : ts
doCommand (StBinOp f) (x:(y:ts)) = f y x : ts
doCommand (StSpecial n f) ts = (f $ take n ts) ++ (drop n ts)

run :: Program a -> Stack a -> Stack a
run [] = id
run (c:cs) = run cs . doCommand c

-- 10 - 1 => push 10 push 1
stackAdd = StBinOp (+)
stackSub = StBinOp (-)
stackMul = StBinOp (*)
stackDiv = StBinOp div
stackMod = StBinOp mod

stackPush x = StSpecial 0 (const [x])

stackNeg = StUnOp negate
stackSwap = StSpecial 2 (\(x:(y:ts)) -> [y, x])
stackDiscard = StSpecial 1 (const [])
stackDupl = StSpecial 1 (\[x] -> [x, x])

program = [stackPush 3, stackPush 2, stackAdd, stackPush 5, stackMul, stackPush 10, stackSub]

