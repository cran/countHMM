\name{fitMod}
\alias{fitMod}
\title{fitMod}
\usage{
fitMod(x,N=2,probs0=NULL,gamma0=NULL,delta0=NULL,stationary=TRUE,lambda=NULL,sup=NULL,m=3,
inflation=NULL)
}
\arguments{
\item{x}{Vector containing the observed time series of counts.}
\item{N}{Integer, number of states. Default is \code{N=2}.}
\item{probs0}{Matrix with \code{N} columns which contains initial parameter values for the state-dependent probabilities for each count. If \code{probs0=NULL} (default), then the initial parameter values are generated automatically (not recommended).}
\item{gamma0}{Initial parameter values for the transition probabilities of the Markov chain underlying the observed counts. Matrix with \code{N} rows and \code{N} columns. If \code{gamma0=NULL} (default), then the initial parameter values are generated automatically (not recommended).}
\item{delta0}{Initial parameter values for the initial probabilities of the Markov chain underlying the observed counts. Vector of length \code{N}. If \code{delta0=NULL} (default), then the stationary distribution is used.}
\item{stationary}{Logical, determines whether the initial distribution of the Markov chain underlying the observed counts is the stationary distribution. Default is \code{stationary=TRUE}.}
\item{lambda}{Vector of length \code{N} which contains the smoothing parameters associated with the different state-dependent distributions. Default is \code{lambda=rep(0,N)}.}
\item{sup}{Integer, determines the upper bound of the support of the state-dependent distributions. If \code{NULL} (default), then the maximum of \code{x} is used.}
\item{m}{Integer, order of the difference penalties. Default is \code{m=3}.}
\item{inflation}{Count probabilities to be excluded from penalization (e.g. in the presence of zero-inflation). Default is \code{inflation=NULL}.}
}
\value{
An object of type countHMM.
}
\references{
Adam, T., Langrock, R., and Weiß, C.H. (2019): Penalized Estimation of Flexible Hidden Markov Models for Time Series of Counts. arXiv:\url{https://arxiv.org/pdf/1901.03275.pdf}.
}
\description{
Estimates the parameters of a hidden Markov model using maximum penalized likelihood estimation. For details, see Adam \emph{et al}. (2019).
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# model fitting
lambda = rep(10^4,2)
fitMod(x=x,lambda=lambda)
}
