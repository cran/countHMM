\name{psRes}
\alias{psRes}
\title{psRes}
\usage{
psRes(mod)
}
\arguments{
\item{mod}{An object of type \code{countHMM} (as returned by the function \code{\link{fitMod}}).}
}
\value{
A matrix with 3 rows, the first of which corresponds to the lower limits, the second of which corresponds to the midpoints, and the third of which corresponds to the upper limits of the pseudo-residual segments. For details, see Zucchini \emph{et al}. (2016).
}
\description{
Computes the pseudo-residuals.
}
\references{
Zucchini W., MacDonald, I.L., and Langrock, R. (2016): Hidden Markov Models for Time Series: An Introduction Using R, 2nd Edition. Chapman & Hall/CRC. doi:\url{https://doi.org/10.1201/b20790}.
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# model fitting
lambda = rep(10^4,2)
mod = fitMod(x=x,lambda=lambda)
# computing the pseudo-residuals
psRes(mod)
}
