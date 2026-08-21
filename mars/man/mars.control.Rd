\name{mars.control}
\alias{mars.control}
\title{Create Control Settings for MARS}
\description{Create a control object containing tuning parameters for the educational MARS implementation.}
\usage{
mars.control(Mmax = 10, d = 3, trace = FALSE)
}
\arguments{
  \item{Mmax}{Maximum forward model size.}
  \item{d}{Penalty parameter used in the lack-of-fit criterion during pruning.}
  \item{trace}{Logical indicating whether algorithm progress should be printed.}
}
\value{An object of class \code{"mars.control"}.}
\author{Shivansh}
