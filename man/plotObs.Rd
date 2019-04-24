\name{plotObs}
\alias{plotObs}
\title{plotObs}
\usage{
plotObs(mod)
}
\arguments{
\item{mod}{An object of type \code{countHMM} (as returned by the function \code{\link{fitMod}}).}
}
\value{
A plot of the Viterbi-decoded time series.
}
\description{
Plots the Viterbi-decoded time series using different colors for the different states.
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# model fitting
lambda = rep(10^4,2)
mod = fitMod(x=x,lambda=lambda)
# plotting the Viterbi-decoded time series
plotObs(mod)
}
