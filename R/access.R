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
    deriv(x[[1L]]) <- value
    return (x)
  }

  if (is.list(value)) {
    if (any(lapply(value, length) != length(x))) stop("There is an error in your deriv assignment")
    attr(x, 'deriv') <- value
    attr(x, 'ddim') <- length(value)
  }
  else {
    ddim <- length(value) %/% length(x)
    if (length(value) %% length(x)) stop("dimensions do not match")

    value <- matrix(value, , ddim)
    attr(x, 'deriv') <- lapply(seq_len(ddim), function(i) value[,i])
    xdim <- dim(x)
    if (!is.null(xdim)) for (d in 1:ddim) dim(attr(x, 'deriv')[[d]]) <- xdim

    attr(x, 'ddim') <- ddim
  }
  class(x) <- 'autograd'
  x
}

## Returns the hessian in simplified form
hessian <- function(x, simplify = TRUE) {
    ddim <- attr(x, 'ddim')
    xdim <- if (is.array(x)) dim(x) else length(x)

    drop(array(unlist (lapply(deriv(x), function(y) unlist(deriv(y)))),
    dim = c(xdim, ddim, ddim)))
}
## Does NOT check for symmetry
`hessian<-` <- function(x, value) {
  ddim <- attr(x, 'ddim')
  if (length(value) != length(x)*ddim*ddim) stop("dimensions do not match")
  value <- array(value, c(length(x), ddim, ddim))
  for (d in 1:ddim) {
    deriv(attr(x, 'deriv')[[d]]) <- value[,,d]
  }
  x
}
