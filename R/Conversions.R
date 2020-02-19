
vector.to.autograd <- function(x, with.hessian = FALSE) {
  xl <- length(x)
  xdim <- if (is.array(x)) dim(drop(x)) else xl
  if(length(xdim) > 1L) warning("vector.to.autograd is intended for one-dimensional vectors")
  deriv(x) <- diag(xl)
  if (with.hessian) hessian(x) <- rep(0, xl*xl*xl)
  x
}