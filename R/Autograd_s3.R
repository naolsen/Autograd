



`+.autograd` <- function(e1, e2) {
  if (missing(e2)) {
    return(e1)
  }
  x <- unclass(e1) + unclass(e2)

  e1 <- autograd.internal(e1)
  e2 <- autograd.internal(e2)

  deriv(x) <- attr(e1, 'deriv') + attr(e2, 'deriv')
  x
}

`-.autograd` <- function(e1, e2) {
  if (missing(e2)) {
    x <- - unclass(e1)
    deriv(x) <- -attr(x,'deriv')
  }
  else {
    x <- unclass(e1) - unclass(e2)
    e1 <- autograd.internal(e1)
    e2 <- autograd.internal(e2)
    deriv(x) <- attr(e1, 'deriv') - attr(e2, 'deriv')
  }
  x
}


`*.autograd` <- function(e1, e2) {

  e1 <- autograd.internal(e1)
  e2 <- autograd.internal(e2)

  ed1 <- attr(e1,'deriv')
  ed2 <- attr(e2,'deriv')

  e1 <- prune.autograd(e1)
  e2 <- prune.autograd(e2)
  x <- e1 * e2

  deriv(x) <- ed1*e2 + ed2*e1
  x
}

`/.autograd` <- function(e1, e2) {

  e1 <- autograd.internal(e1)
  e2 <- autograd.internal(e2)

  ed1 <- attr(e1,'deriv')
  ed2 <- attr(e2,'deriv')

  e1 <- prune.autograd(e1)
  e2 <- prune.autograd(e2)

  x <- e1 / e2
  deriv(x) <- (ed1*e2 - ed2*e1) / (e2*e2)
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

  x <- autograd.internal(x)
  y <- autograd.internal(y)

  ed1 <- attr(x,'deriv')
  ed2 <- attr(y,'deriv')

  x <- prune.autograd(x)
  y <- prune.autograd(y)

  z <- x %*% y
  deriv(z) <- ed1 %*% y + x %*% ed2
  z
}
