## Print functions

print.autograd <- function(x, ...) {
  dx <- attr(x, 'deriv')
  ddim <- attr(x, 'ddim')
  attr(x, 'deriv') <- attr(x, 'ddim') <- NULL

  print(unclass(x), ...)

  cat("Derivative: ddim =", ddim, "\n")
  if (ddim > 1) for (i in 1:ddim) {
    cat('[[', i, ']]\n', sep="")
    print(dx[[i]], ...)
  }
  else print(dx[[1L]], ...)
  cat('\n')
}

print.deriv <- function(x) {
  unlist(deriv(x))
  ddim <- attr(x, 'ddim')
  xdim <- if (is.array(x)) dim(x) else length(x)

  drop(array(unlist(deriv(x)), dim = c(xdim, ddim)))
}