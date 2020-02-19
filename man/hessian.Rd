\name{hessian}
\alias{hessian}
\title{Read/write to 2nd order derivative/hessian}
\usage{
hessian(x)

hessian(x) <- value
}
\arguments{
\item{x}{Autograd object with \code{ddim} derivative arguments.
For the read method, all 2nd derivatives must exist}
\item{value}{Array of dimension \code{c(dim(x), ddim, ddim)}}
}
\value{
\code{hessian}:
Hessian of \code{x} wrapped up in an array of dim \code{c(dim(x), ddim, ddim)} with dimensions possibly dropped.
\code{hessian<-}: Autograd object with all 2nd derivatives defined.
}
\details{
Note: there are no symmetry checks on \code{value} when assigning a hessian to \code{x}.
}
\seealso{
\link{deriv}
}
