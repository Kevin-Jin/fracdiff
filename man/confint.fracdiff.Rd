\name{confint.fracdiff}
\alias{confint.fracdiff}
\title{Confidence Intervals for Fracdiff Model Parameters}
\description{
  Computes confidence intervals for  one or more parameters in a fitted
  fracdiff model, see \code{\link{fracdiff}}.
}
\usage{
\method{confint}{fracdiff}(object, parm, level = 0.95, \dots)
}
\arguments{
  \item{object}{an object of class \code{fracdiff}, typically result of
    \code{\link{fracdiff}(..)}.}
  \item{parm}{a specification of which parameters are to be given
    confidence intervals, either a vector of numbers or a vector of
    names.  If missing, all parameters are considered.}
  \item{level}{the confidence level required.}
  \item{\dots}{additional argument(s) for methods.}
}
\value{
  A matrix (or vector) with columns giving lower and upper confidence
  limits for each parameter. These will be labelled as (1-level)/2 and
  1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\author{Spencer Graves posted the initial version to R-help.}
\seealso{the generic \code{\link{confint}}; \code{\link{fracdiff}} model
  fitting.
}
\examples{
set.seed(101)
ts2 <- fracdiff.sim(5000, ar = .2, ma = -.4, d = .3)
mFD <- fracdiff( ts2$series, nar = length(ts2$ar), nma = length(ts2$ma))
coef(mFD)
confint(mFD)
}
\keyword{models}

