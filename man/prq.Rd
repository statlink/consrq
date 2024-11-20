\name{Positively constrained quantile regression}
\alias{prq}
\alias{mprq}
\title{
Positively constrained quantile regression
}

\description{
Positively constrained quantile regression.
}

\usage{
prq(y, x, tau = 0.5)
mprq(y, x, tau = 0.5)
}

\arguments{
\item{y}{
The response variable. For the prq() a numerical vector with observations, but for the mprq() a numerical matrix .
}
\item{x}{
A matrix with independent variables, the design matrix.
}
\item{tau}{
The quantile(s) to be estimated, a number strictly between 0 and 1. It a vector of values between 0 and 1; in this case an object of class "rqs" is returned containing among other things a matrix of coefficient estimates at the specified quantiles.
}
}

\details{
The constraint is that all beta coefficients (including the constant) are non negative. That is,
\eqn{min \sum_{i=1}^n|y_i-\bm{x}_i^\top\bm{\beta}|} such that \eqn{\beta_j \geq 0}.
The pls() function performs a single regression model, whereas the mpls() function performs a regression for each column of y. Each regression is independent of the others.
}

\value{
A list including:
\item{be}{
A numerical matrix with the positively constrained beta coefficients.
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
\code{\link{pcrq}
}
}

\examples{
x <- as.matrix( iris[1:50, 1:4] )
y <- rnorm(50)
prq(y, x)
}
