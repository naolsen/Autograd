\name{Autograd-operator}
\alias{Autograd-operator}
\alias{Autograd-function}
\title{Operator and function overloading by autograd class}
\usage{
e1 + e2

e1 - e2

exp(e1)

etc.
}
\arguments{
\item{e1,e2}{At least one must be an object of class \code{autograd}.}
}
\description{
Many functions and operators are overloaded by the autograd class. Using smart bookkeeping and differential algebra rules, higher-order derivatives are also supported.
}
\details{
List of overloaded functions/operators:
'\code{+}', '\code{-}', '\code{*}', '\code{/}', '\code{\%*\%}', '\code{t}', `\code{sqrt}`, `\code{log}`, `\code{exp}`, `\code{sum}`, `\code{mean}`,
`\code{rep}`, `\code{solve}`, `\code{determinant}`, `\code{print}`,
subsetting (only read) `\code{[}`, trigonometric and hyperbolic functions.
}
\seealso{
\link{deriv}
}
