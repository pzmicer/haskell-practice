-- Algebraic expressions
import Data.Map


type VarBinding a = Map String a

data Expr a
    = Const a
    | Var String -- variable name 
    | UnOp (a -> a) (Expr a)
    | BinOp (a -> a -> a) (Expr a) (Expr a)


eval :: Expr a -> VarBinding a -> a
eval (Const x) _ = x
eval (Var s) m = m ! s
eval (UnOp f e) m = f (eval e m)
eval (BinOp f e1 e2) m = f (eval e1 m) (eval e2 m)

tree = BinOp (+) (UnOp sin (BinOp (/) (Const 3.14159) (Var "k"))) (Var "k")
bindings = fromList [("k", 1.2)]