\name{Autograd-class}
\alias{Autograd-class}
\title{Automatic differentation in R}
\description{
Autograd objects are objects of class \code{autograd}. They shall have attributes \code{deriv}, which holds the derivative, and \code{ddim}, which is the number of derivative arguments.
\code{deriv} shall be a list of length \code{ddim}.
Higher-order derivatives are handled by making the derivative into an autograd object itself, thus nesting the derivatives.
}
\seealso{
\link{deriv}, \link{Autograd-operator}
}
