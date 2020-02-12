## Print functions

print.autograd <- function(x) {
  dx <- attr(x, 'deriv')
  attr(x, 'deriv') <- NULL
  print(unclass(x))
  cat("Derivative:\n")
  print(dx)
}
