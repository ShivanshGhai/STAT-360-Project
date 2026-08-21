\name{mars}
\alias{mars}
\title{Fit a Multivariate Adaptive Regression Splines Model}
\description{
Fit an educational implementation of the Multivariate Adaptive Regression Splines
(MARS) algorithm using a forward stepwise construction followed by backward pruning.
}
\usage{
mars(formula, data, control = mars.control())
}
\arguments{
  \item{formula}{A model formula describing the response and explanatory variables.}
  \item{data}{A data frame containing the variables in the model.}
  \item{control}{A \code{mars.control} object controlling the maximum model size,
  pruning penalty, and tracing behaviour.}
}
\details{
The fitting process first builds a large basis expansion with \code{fwd_stepwise()},
then prunes it using \code{bwd_stepwise()} and a lack-of-fit criterion inspired by
MARS generalized cross-validation. The final coefficients are estimated with
least squares using \code{lm()} on the selected basis matrix.
}
\value{
A fitted object of class \code{"mars"} that also inherits from \code{"lm"}. The
returned object contains the fitted basis matrix, basis function definitions,
coefficients, fitted values, residuals, and the usual linear-model components.
}
\author{Shivansh}
\references{
Friedman, J. H. (1991). Multivariate Adaptive Regression Splines.
\emph{The Annals of Statistics}, 19(1), 1--67.
}
\seealso{
\code{mars.control}, \code{predict.mars}, \code{print.mars}, \code{summary.mars},
\code{plot.mars}, \code{anova.mars}
}
\examples{
data(marstestdata)
mc <- mars.control(Mmax = 10)
fit <- mars(y ~ ., data = marstestdata, control = mc)
print(fit)
predict(fit)
summary(fit)
}
