
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

## Convert one argument functions to autograd
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

log.autograd <- fun.to.autograd.deriv(log, function(x) 1/x)
exp.autograd <- fun.to.autograd.deriv(exp, exp)
