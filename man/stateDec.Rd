\name{stateDec}
\alias{stateDec}
\title{stateDec}
\usage{
stateDec(mod)
}
\arguments{
\item{mod}{An object of type \code{countHMM} (as returned by the function \code{\link{fitMod}}).}
}
\value{
A vector of Viterbi-decoded states.
}
\description{
Decodes the states of the Markov chain underlying the observed time series of counts using the Viterbi algorithm. For details, see Zucchini \emph{et al}. (2016).
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
# decoding the states
stateDec(mod)
}
