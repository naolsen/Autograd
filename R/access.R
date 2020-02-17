#' Derivate
#'
#' @description Access/read derivative
#'
#' @param x
#' @param value
#'
#' @rdname deriv
#' @return
#' @export
#'
deriv <- function(x, d) {
  if (missing(d)) attr(x, 'deriv')
  else attr(x, 'deriv')[[d]]
}

`deriv<-` <- function(x, value) {
  if (is.list(x) && length(x) == 1L) {
    deriv(x[[1]]) <- value
    return (x)
  }
  if (!is.list(value) && length(value) != length(x)) stop("There is an error in your deriv assignment (autograd)")

  if (is.list(value)) {
    if (any(lapply(value, length) != length(x))) stop("There is an error in your deriv assignment (autog)")
    attr(x, 'deriv') <- value
    attr(x, 'ddim') <- length(value)
  }
  else {
    attr(x, 'deriv') <- list(value)
    attr(x, 'ddim') <- 1L
  }
  class(x) <- 'autograd'
  x
}

## Returns the hessian in simplified form
hessian <- function(x, simplify = TRUE) {
    ddim <- attr(x, 'ddim')
    xdim <- if (is.array(x)) dim(x) else length(x)
    for (d in 1:ddim) {
      unlist(deriv(deriv(x, d)))
    }
    drop(array(unlist(lapply(deriv(x), function(y) unlist(deriv(y)))),
    dim = c(xdim, ddim, ddim)))
}