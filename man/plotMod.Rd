\name{plotMod}
\alias{plotMod}
\title{plotMod}
\usage{
plotMod(mod)
}
\arguments{
\item{mod}{An object of type \code{countHMM} (as returned by the function \code{\link{fitMod}}).}
}
\value{
A plot of the estimated state-dependent distributions.
}
\description{
Plots the estimated state-dependent distributions.
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# model fitting
lambda = rep(10^4,2)
mod = fitMod(x=x,lambda=lambda)
# plotting the estimated state-dependent distributions
plotMod(mod)
}
