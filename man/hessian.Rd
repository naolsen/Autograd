\name{hessian}
\alias{hessian}
\title{Print-friendly Hessian}
\usage{
hessian(x)
}
\arguments{
\item{x}{Autograd object for which all 2nd derivatives exist (as defined by the \code{ddim} attribute)}
}
\value{
Hessian of \code{x} wrapped up in an array of dim \code{c(dim(x), ddim, ddim)} with dimensions possibly dropped.
}
\seealso{
\link{deriv}
}
