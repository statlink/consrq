\name{Positive and unit sum constrained quantile regression}
\alias{pcrq}
\alias{mpcrq}
\title{
Positive and unit sum constrained quantile regression
}

\description{
Positive and unit sum constrained quantile regression.
}

\usage{
pcrq(y, x, tau = 0.5)
mpcrq(y, x, tau = 0.5)
}

\arguments{
\item{y}{
The response variable. For the pcrq() a numerical vector with observations, but for the mpcrq() a numerical matrix.
}
\item{x}{
A matrix with independent variables, the design matrix.
}
\item{tau}{
The quantile(s) to be estimated, a number strictly between 0 and 1. It a vector of values between 0 and 1; in this case an object of class "rqs" is returned containing among other things a matrix of coefficient estimates at the specified quantiles.
}
}

\details{
The constraint is that all beta coefficients are positive and sum to 1. That is,
i.e. \eqn{min \sum_{i=1}^n(y_i-\bm{x}_i^\top\bm{\beta})^2} such that \eqn{\beta_j \geq 0} and \eqn{\sum_{j=1}^d\beta_j=1}. The pcrq() function performs a single regression model, whereas the mpcrq() function performs a regression for each column of y. Each regression is independent of the others.
}

\value{
A list including:
\item{be}{
A numerical matrix with the positively constrained beta coefficients.
}
\item{mae}{
A numerical vector with the mean absolute error.
}
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{prq}
}
}

\examples{
x <- as.matrix( iris[1:50, 1:4] )
y <- rnorm(50)
pcrq(y, x)
}
