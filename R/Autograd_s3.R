



`+.autograd` <- function(e1, e2) {
  x <- unclass(e1) + unclass(e2)

  e1 <- autograd.internal(e1)
  e2 <- autograd.internal(e2)

  deriv(x) <- attr(e1, 'deriv') + attr(e2, 'deriv')
  x
}

`-.autograd` <- function(e1, e2) {
  if (missing(e2)) {
    x <- - unclass(e1)
    attr(x,'deriv') <- -attr(x,'deriv')
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

  attr(x,'deriv') <- (ed1*e2 - ed2*e1) / (e2*e2)
  class(x) <- 'autograd'
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
  attr(z,'deriv') <- ed1 %*% y + x %*% ed2
  class(z) <- 'autograd'
  z
}

`[.autograd` <- function(x, ...) {

  x2 <- unclass(x)[...]
  deriv(x2) <- deriv(x)[...]
  x2
}

t.autograd <- function(x) {

  x2 <- t(unclass(x))
  deriv(x2) <- t(deriv(x))
  x2
}