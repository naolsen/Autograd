\name{vector_to_autograd}
\alias{vector.to.autograd}
\title{Assign "standard" derivative to vector}
\description{Assign "standard" derivative to vector}
\usage{
vector.to.autograd(x, with.hessian = FALSE)
}
\arguments{
\item{x}{Vector}

\item{with.hessian}{Also assign a  hessian?}
}
\value{
Autograd version of \code{x} with default 1st derivative and (possibly) hessian/2nd derivative.
}
\details{
This is the default way of assigning a derivate to an arbitrary input vector, that is, the gradient is the identity matrix, and the hessian is zero.
}
\seealso{
\link{deriv}, \link{hessian}
}