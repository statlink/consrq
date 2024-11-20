\name{Lower and upper bound constrained quantile regression}
\alias{int.crq}
\alias{int.mcrq}
\title{
Lower and upper bound constrained quantile regression
}

\description{
Lower and upper bound constrained quantile regression.
}

\usage{
int.crq(y, x, tau = 0.5, lb, ub)
int.mcrq(y, x, tau = 0.5, lb, ub)
}

\arguments{
\item{y}{
For the int.crq() the response variable, a numerical vector with observations, but a matrix of response variables for the int.mcrq().
}
\item{x}{
A matrix with independent variables, the design matrix.
}
\item{tau}{
The quantile(s) to be estimated, a number strictly between 0 and 1. It a vector of values between 0 and 1; in this case an object of class "rqs" is returned containing among other things a matrix of coefficient estimates at the specified quantiles.
}
\item{lb}{
A vector or a single value with the lower bound(s) in the coefficients.
}
\item{ub}{
A vector or a single value with the upper bound(s) in the coefficients.
}
}

\details{
This function performs quantile regression under the constraint that the beta coefficients lie within interval(s), i.e. \eqn{min \sum_{i=1}^n|y_i-\bm{x}_i^\top\bm{\beta}|} such that \eqn{lb_j\leq \beta_j \leq ub_j}.
}

\value{
A list including:
\item{be}{
A numerical matrix with the constrained beta coefficients.
}
\item{mae}{
A numerical vector with the mean absolute error(s).
}
}

%\references{
%Hansen, B. E. (2022). Econometrics, Princeton University Press.
%}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{prq}, \link{pcrq}
}
}

\examples{
x <- as.matrix( iris[1:50, 1:4] )
y <- rnorm(50)
int.crq(y, x, lb = -0.2, ub = 0.2)
}
