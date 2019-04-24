\name{plotRes}
\alias{plotRes}
\title{Quantile-quantile and autocorrelation function plots of the pseudo-residuals.}
\usage{
plotRes(mod)
}
\arguments{
\item{mod}{An object of type \code{countHMM} (as returned by the function \code{\link{fitMod}}).}
}
\value{
A plot with two windows, the first of which displays the quantile-quantile function and the second of which displays the autocorrelation function of the pseudo-residuals.
}
\description{
Plots quantile-quantile and autocorrelation function plots of the midpoints of the pseudo residual segments. For details, see \cite{Zucchini et al. (2016)}.
}
\references{
Zucchini W., MacDonald, I.L., and Langrock, R. (2016): \emph{Hidden Markov models for time series: An introduction using R, 2nd edition}. Chapman & Hall/CRC, Boca Raton.
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# model fitting
lambda = rep(10^4,2)
mod = fitMod(x=x,lambda=lambda)
# plotting the pseudo residuals
plotRes(mod)
}
