type Stack a = [a]
type Command a = Stack a -> Stack a
type Program a = [Command a]

run :: Program a -> Stack a -> Stack a
run [] = id
run (c:cs) = run cs . c


-- 10 - 1 => push 10 push 1
stackAdd (x:(y:ss)) = (y + x) : ss
stackSub (x:(y:ss)) = (y - x) : ss
stackMul (x:(y:ss)) = (y * x) : ss
stackDiv (x:(y:ss)) = (y `div` x) : ss
stackMod (x:(y:ss)) = (y `mod` x) : ss
stackPush x = (x:)
stackNeg (x:ss) = ss
stackDupl st@(x:_) = x : st

program = [stackPush 3, stackPush 2, stackAdd, stackPush 5, stackMul, stackPush 10, stackSub]

