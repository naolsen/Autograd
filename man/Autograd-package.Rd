\docType{package}
\name{Autograd-package}
\alias{Autograd}
\alias{Autograd-package}
\title{Automatic differentation in R}
\description{
Native implementation of automatic differentation. Implemented as S3 methods on class \code{autograd}. Works with derivatives of any order.
}
\details{
Autograd overloads many standard operators and functions using the S3 framework.
Mix scalars, vectors, array etc. as usual in R. The package does NOT contain any optimizer, and is not made for speed.
The most standard way of converting a vector to an Autograd object is using \link{vector.to.autograd}.
}
\seealso{
\link{Autograd-class}
}