## Print functions

print.autograd <- function(x) {
  if (!is.null(attr(x, 'ddim')))  print.autog(x)
  else {
    dx <- attr(x, 'deriv')
    attr(x, 'deriv') <- NULL
    print(unclass(x))
    cat("Derivative:\n")
    print(dx)
}}

print.autog <- function(x) {
  dx <- attr(x, 'deriv')
  ddim <- attr(x, 'ddim')
  attr(x, 'deriv') <- attr(x, 'ddim') <- NULL

  print(unclass(x))

  cat("Derivative: ddim =", ddim, "\n")
  if (ddim > 1) for (i in 1:ddim) {
    cat('[[', i, ']]\n', sep="")
    print(dx[[i]])
  }
  else print(dx[[1L]])
  cat('\n')
}

