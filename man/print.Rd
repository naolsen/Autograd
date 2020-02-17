\name{print}
\alias{print}
\alias{print.autograd}
\title{Print autograd object}
\usage{
print(x)
}
\arguments{
\item{x}{Object of class \code{autograd}}
}
\description{
Prints \code{x}, along with derivative(s). If the number of derivative arguments in 1, the list indicator is silently dropped.
Use \link{hessian} for a user-friendly 2nd derivative.
}
