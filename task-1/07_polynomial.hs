polynomialHelper [] x p res = res
polynomialHelper (c:cs) x p res = polynomialHelper cs x (p * x) (res + c * p)
evalPolynomial cs x = polynomialHelper (reverse cs) x 1 0