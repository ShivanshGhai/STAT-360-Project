\name{algorithms}
\alias{fwd_stepwise}
\alias{bwd_stepwise}
\alias{LOF}
\title{Helper Algorithms for the Educational MARS Package}
\description{Internal-facing helper functions used to construct, prune, and score basis expansions for the MARS model.}
\usage{
fwd_stepwise(y, x, control = mars.control())
bwd_stepwise(fwd, control)
LOF(formula, data, control)
}
\arguments{
  \item{y}{Numeric response vector.}
  \item{x}{Predictor matrix.}
  \item{control}{A \code{mars.control} object.}
  \item{fwd}{A forward-pass object containing \code{y}, \code{B}, and \code{Bfuncs}.}
  \item{formula}{A model formula used for scoring lack of fit.}
  \item{data}{A data frame containing the response and basis matrix.}
}
\value{
For \code{fwd_stepwise()} and \code{bwd_stepwise()}, a list with components \code{y}, \code{B}, and \code{Bfuncs}. For \code{LOF()}, a numeric lack-of-fit score.
}
\author{Shivansh}
