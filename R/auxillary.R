
prune.autograd <- function(x) {
  if (!is.null(attr(x, 'deriv'))) {
    attr(x, 'deriv') <- prune.autograd(attr(x, 'deriv'))
    if (is.null(attr(x, 'deriv'))) x <- unclass(x)
  }
  else x <- NULL
  x
}

autograd.internal <- function(x) {
  if(!inherits(x, 'autograd')) {
    attr(x, 'deriv') <- x*0
    x
  }
  else x
}

## Convert one argument functions to autograd using chain rule
fun.to.autograd.deriv <- function(fun, fderiv) {

  f <- function(x) {
    ed1 <- attr(x,'deriv')
    x <- prune.autograd(x)

    x2 <- fun(x)
    deriv(x2) <- ed1*fderiv(x)
    x2
  }
  f
}

## Convert one argument functions to autograd using recursion
rfun.to.autograd.deriv <- function(fun) {
  f <- function(x, ...) {
    x2 <- fun(unclass(x), ...)
    deriv(x2) <- fun(deriv(x), ...)
    x2
  }
  f
}

## differentiable functions
log.autograd <- fun.to.autograd.deriv(log, function(x) 1/x)
exp.autograd <- fun.to.autograd.deriv(exp, exp)
sin.autograd <- fun.to.autograd.deriv(sin, cos)
cos.autograd <- fun.to.autograd.deriv(cos, function(x) -sin(x))
sqrt.autograd <- fun.to.autograd.deriv(sqrt, function(x) 1/(2*sqrt(x)))

## other functions
`[.autograd` <- rfun.to.autograd.deriv(`[`)
t.autograd <- rfun.to.autograd.deriv(t)
rep.autograd <- rfun.to.autograd.deriv(rep)
sum.autograd <- rfun.to.autograd.deriv(sum)