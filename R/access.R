
deriv <- function(x, d) {
  if (missing(d)) attr(x, 'deriv')
  else attr(x, 'deriv')[[d]]
}

`deriv<-` <- function(x, value) {
  if (!is.list(value) && length(value) != length(x)) stop("There is an error in your deriv assignment (autograd)")
  attr(x, 'deriv') <- value
  class(x) <- 'autograd'
  if (is.list(value)) {
    if (any(lapply(value, length) != length(x))) stop("There is an error in your deriv assignment (autog)")
    attr(x, 'ddim') <- length(value)
    class(x) <- 'autog'
  }
  x
}
