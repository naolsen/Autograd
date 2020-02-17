\name{deriv}
\alias{deriv}
\alias{derivative}
\title{Access/read derivative}
\usage{
deriv(x)

deriv(x) <- value
}
\arguments{
\item{x}{Element}

\item{value} {Value of derivative. Dimensions must match those of \code{x}}

}
\value{
\code{deriv(x)}: The derivative of an autograd object

\code{deriv(x)<-}: An object of class autograd
}
\details{These functions can be nested, e.g. \code{deriv(deriv(x)) <- value}, will assign \code{value} as the 2nd derivative of
\code{x}.
}
