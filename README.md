# Autograd
# R package 'Autograd'

Automatic differentation.

Implemented as S3 methods. Works with derivatives of any order.



Use ``deriv`` for reading/writing to derivative. Can be nested.

Current functions/operators overloaded:
`+`, `-`, `*`, `/`, `%*%`, `t`, `sqrt`, `log`, `exp`, `sin`, `cos`, `sum`, `mean`,
`rep`, `solve`, `print`, 
subsetting (only read) `[`. 

### Known issues
`solve` does not work as intended on `solve(a, b)`. Use `solve(a) %*% b` instead.


### Example
Try calculate the numerical derivatives for comparison
```
> s <- 3
> deriv(s) <- 1
> deriv(deriv(s)) <- 0
> print(s)
[1] 3
Derivative: ddim = 1 
[1] 1
Derivative: ddim = 1 
[1] 0

## Polynomial example
> s*(s+2) - 3 ## Gives value and two derivatives of polynomial x*(x+2) - 3 in x = 3.
[1] 12
Derivative: ddim = 1 
[1] 8
Derivative: ddim = 1 
[1] 2

## Exponential example
> exp(s)
[1] 20.08554
Derivative: ddim = 1 
[1] 20.08554
Derivative: ddim = 1 
[1] 20.08554

## Complicated example
> s2 <- log(s + 3) -  s*s + 4*exp(s/2)
> s2
[1] 10.71852
Derivative: ddim = 1 
[1] 3.130045
Derivative: ddim = 1 
[1] 2.453911

> s2 / s2 ## Constant expression, thus derivatives must zero.
[1] 1
Derivative: ddim = 1 
[1] 0
Derivative: ddim = 1 
[1] 0

## Matrix example with quadratic form
## Here n is treated as a constant, and w depends on an independent variable.
> m <- matrix(1:16, 4, 4)
> n <- t(m) %*% m
> w <- 1:4
> deriv(w) <- c(0, 1, -1, 0)

> t(w) %*% n %*% w
      [,1]
[1,] 44600
Derivative: ddim = 1 
      [,1]
[1,] -3360
```
Multivariate examples
```
> s <- 3
> deriv(s) <- c(1, 3)
> s
[1] 3
Derivative: ddim = 2 
[[1]]
[1] 1
[[2]]
[1] 3

> s / s
[1] 1
Derivative: ddim = 2 
[[1]]
[1] 0
[[2]]
[1] 0

> ww <- vector.to.autograd(1:4, with.hessian = T)

## ww is too long to print here, so print value and deriv seperately:
> as.numeric(ww)
[1] 1 2 3 4
> print.deriv(ww)
     [,1] [,2] [,3] [,4]
[1,]    1    0    0    0
[2,]    0    1    0    0
[3,]    0    0    1    0
[4,]    0    0    0    1

## Quadratic form:
> t(ww) %*% n %*% ww
      [,1]
[1,] 44600
Derivative: ddim = 4 
[[1]]
     [,1]
[1,] 2200
Derivative: ddim = 4 
[[1]]
     [,1]
[1,]   60
[[2]]
     [,1]
[1,]  140
[[3]]
     [,1]
[1,]  220
[[4]]
     [,1]
[1,]  300

[[2]]
     [,1]
[1,] 5560
Derivative: ddim = 4 
[[1]]
     [,1]
[1,]  140
[[2]]
     [,1]
[1,]  348
[[3]]
     [,1]
[1,]  556
[[4]]
     [,1]
[1,]  764

[[3]]
     [,1]
[1,] 8920
Derivative: ddim = 4 
[[1]]
     [,1]
[1,]  220
[[2]]
     [,1]
[1,]  556
[[3]]
     [,1]
[1,]  892
[[4]]
     [,1]
[1,] 1228

[[4]]
      [,1]
[1,] 12280
Derivative: ddim = 4 
[[1]]
     [,1]
[1,]  300
[[2]]
     [,1]
[1,]  764
[[3]]
     [,1]
[1,] 1228
[[4]]
     [,1]
[1,] 1692
## Let's get a better understanding of the derivatives:
print.deriv(t(ww) %*% n %*% ww)
[1]  2200  5560  8920 12280
> hessian(t(ww) %*% n %*% ww)
     [,1] [,2] [,3] [,4]
[1,]   60  140  220  300
[2,]  140  348  556  764
[3,]  220  556  892 1228
[4,]  300  764 1228 1692


## We see that gradient is not zero for c(1,2,3,4).
## By changing the values to c(0, 1, -2, 1) we will get a minimum for the quadratic form. 
## As expected, the gradient is zero:
> ww[1:4] <- c(0, 1, -2, 1)
> as.numeric(t(ww) %*% n %*% ww)
[1] 0
> print.deriv(t(ww) %*% n %*% ww)
[1] 0 0 0 0
> hessian(t(ww) %*% n %*% ww)
     [,1] [,2] [,3] [,4]
[1,]   60  140  220  300
[2,]  140  348  556  764
[3,]  220  556  892 1228
[4,]  300  764 1228 1692
## This is a quadratic form, so the 2nd derivative is unchanged.

```
