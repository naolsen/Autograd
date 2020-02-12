# Autograd
# R package 'Autograd'

Automatic differentation.

Implemented as S3 methods. Works with derivatives of any order.



Use ``deriv`` for reading/writing to derivative. Can be nested.

Current functions/operators overloaded:
`+`, `-`, `*`, `/`, `%*%`, `t`, `log`, `exp`, `print`, subsetting (only read). 

### Example
Try calculate the numerical derivatives for comparison
```
> s <- 3
> deriv(s) <- 1
> deriv(deriv(s)) <- 0
> print(s)
[1] 3
Derivative:
[1] 1
Derivative:
[1] 0

## Polynomial example
> s*(s+2) - 3 ## Gives value and two derivatives of polynomial x*(x+2) - 3 in x = 3.
[1] 12
Derivative:
[1] 8
Derivative:
[1] 2

## Exponential example
> exp(s)
[1] 20.08554
Derivative:
[1] 20.08554
Derivative:
[1] 20.08554

## Complicated example
> s2 <- log(s + 3) -  s*s + 4*exp(s/2)
> s2 / s2 ## Constant expression, thus derivatives must zero.
[1] 1
Derivative:
[1] 0
Derivative:
[1] 0

## Matrix example with quadratic form
## Here n is treated as a constant, and w depends on an independent variable.
> m <- matrix(1:16, 4, 4)
> n <- t(m) %*% m
## m is treated as a constant
> w <- 1:4
> deriv(w) <- c(0, 1, -1, 0)

> t(w) %*% n %*% w
      [,1]
[1,] 44600
Derivative:
      [,1]
[1,] -3360
```

