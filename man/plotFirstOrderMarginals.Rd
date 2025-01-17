% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotFirstOrderMarginals}
\alias{plotFirstOrderMarginals}
\title{Function to visualize the first order marginals}
\usage{
plotFirstOrderMarginals(
  permutations,
  type = "ranking",
  decreasing = FALSE,
  smoothed = FALSE
)
}
\arguments{
\item{permutations}{a list of objects of class \code{\linkS4class{Permutation}}}

\item{smoothed}{a logical value to indicate whether the estimation should
be smoothed (\code{FALSE} by default)}
}
\value{
An object of class ggplot with the visualization
}
\description{
This function estimates from a list of permutations the first
order marginals and visualizes them
}
\details{
This function requires two additional packages, ggplot2 and reshape2
}
\examples{
scores <- matrix(rnorm(100, 1, 3), ncol=10)
permus <- getPermutationFromScores (scores)
plotFirstOrderMarginals(permus)

}
