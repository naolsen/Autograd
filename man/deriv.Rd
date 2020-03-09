\name{deriv}
\alias{deriv}
\alias{deriv<-}
\alias{derivative}
\alias{print_deriv}
\title{Autograd derivative}
\description{Access/read derivative}
\usage{
deriv(x)

deriv(x, d)

print_deriv(x)

deriv(x) <- value
}
\arguments{
\item{x}{Element}

\item{value}{Value of derivative. Either a list, or a numeric whose length is a multiple of \code{length(x)}.}

\item{d}{Deriv index. (Optional)}
}
\value{
\code{deriv(x)}: The derivative of an autograd object (a list).

\code{print_deriv(x)}: Pretty-formatted derivative. Drops higher-order derivatives.

\code{deriv(x)<-}: An object of class autograd
}
\details{
If \code{value} is a list, then all elements must have the same length as \code{x}.
The \code{ddim} attribute is set as the length of \code{value} if this is a list and \code{length(value)/length(x)} otherwise.
These functions can be nested, e.g. \code{deriv(deriv(x)[[1]]) <- value},
will assign \code{value} as the 2nd derivative of \code{x}.
Update: \link{hessian} is the recommended way to assign a 2nd derivative.



Use \link{hessian} for a print-friendly read of the 2nd derivative or \code{deriv(deriv(x)[[i]])} etc..
}
