



`+.autograd` <- function(e1, e2) {
  if (missing(e2)) {
    return(e1)
  }
  x <- unclass(e1) + unclass(e2)

  e1 <- autograd.internal(e1, allow.list = TRUE)
  e2 <- autograd.internal(e2, allow.list = TRUE)

  deriv(x) <- mapply('+', attr(e1, 'deriv'), attr(e2, 'deriv'), SIMPLIFY = FALSE)
  x
}

`-.autograd` <- function(e1, e2) {
  if (missing(e2)) {
    x <- - unclass(e1)
    deriv(x) <- lapply(attr(x,'deriv'), '-')
  }
  else {
    x <- unclass(e1) - unclass(e2)
    e1 <- autograd.internal(e1, allow.list = TRUE)
    e2 <- autograd.internal(e2, allow.list = TRUE)

    deriv(x) <- mapply('-', attr(e1, 'deriv'), attr(e2, 'deriv'), SIMPLIFY = FALSE)
  }
  x
}


`*.autograd` <- function(e1, e2) {

  e1 <- autograd.internal(e1, allow.list = TRUE)
  e2 <- autograd.internal(e2, allow.list = TRUE)

  ed1 <- attr(e1,'deriv')
  ed2 <- attr(e2,'deriv')

  e1 <- prune.autograd(e1)
  e2 <- prune.autograd(e2)
  x <- e1 * e2

  deriv(x) <- mapply(function(a,b) a*e2 + b*e1, ed1, ed2, SIMPLIFY = FALSE)
  x
}

`/.autograd` <- function(e1, e2) {

  e1 <- autograd.internal(e1, allow.list = TRUE)
  e2 <- autograd.internal(e2, allow.list = TRUE)

  ed1 <- attr(e1,'deriv')
  ed2 <- attr(e2,'deriv')

  e1 <- prune.autograd(e1)
  e2 <- prune.autograd(e2)

  x <- e1 / e2
  deriv(x) <- mapply(function(a,b) (a*e2 - b*e1) / (e2*e2), ed1, ed2, SIMPLIFY = FALSE)
  x
}

`%*%` <- function(x,...) UseMethod("%*%", x)
`%*%.default` <- function(x,y)  {
  if(inherits(y, 'autograd')) {
    `%*%.autograd`(x,y)
  }
  else  .Primitive("%*%")(x,y)
}

`%*%.autograd` <- function (x,y) {

  x <- autograd.internal(x, allow.list = TRUE)
  y <- autograd.internal(y, allow.list = TRUE)

  ed1 <- attr(x,'deriv')
  ed2 <- attr(y,'deriv')

  x <- prune.autograd(x)
  y <- prune.autograd(y)

  z <- x %*% y
  deriv(z) <- mapply(function(a,b) a %*% y + x %*% b,
                             ed1, ed2, SIMPLIFY = FALSE)
  z
}

solve.autograd <- function(a, b, ...) {
  if (!missing(b)) solve(a, ...) %*% b
  else {
    x <- prune.autograd(a)
    x2 <- solve(x, ...)
    deriv(x2) <- lapply(deriv(a), function(x) x2 %*% x %*% x2)
    x2
  }
}

## Note: the derivative is here an attribute to modulus
determinant.autograd <- function(x, logarithm = TRUE, ...) {
  if (! logarithm) stop("Only implemented for logarithm = TRUE")
  dx <- deriv(x)
  x <- prune.autograd(x)
  x2inv <- solve(x, ...)
  x2 <- determinant(x, TRUE, ...)

  deriv(x2$modulus) <- lapply(dx, function(a) sum(diag(x2inv %*% a)))
  x2
}
