\name{nLogLike}
\alias{nLogLike}
\title{Penalized negative log-likelihood}
\usage{
nLogLike(parvect,x,N,stationary,lambda,sup,m,inflation)
}
\arguments{
\item{parvect}{Vector of working parameters (as returned by \code{\link{pn2pw}}).}
\item{x}{Vector of observed counts.}
\item{N}{Integer, number of states.}
\item{stationary}{Logical, determines whether the initial distribution of the Markov chain underlying the observed counts is the stationary distribution.}
\item{lambda}{Vector of length \code{N} which contains the smoothing parameters associated with the different state-dependent distributions.}
\item{sup}{Integer, determines the upper bound of the support of the state-dependent distributions. If \code{NULL}, then the maximum of \code{x} is used.}
\item{m}{Integer, order of the difference penalties.}
\item{inflation}{Count probabilities to be excluded from penalization (e.g. in the presence of zero-inflation).}
}
\value{
Numeric, the penalized negative log-likelihood.
}
\description{
Computes the penalized negative log-likelihood using the forward algorithm as described in Adam \emph{et al}. (2019). Not intended to be run by the user (internal function, called by the function \code{\link{fitMod}}).
}
\references{
Adam, T., Langrock, R., and Weiß, C.H. (2019): Penalized Estimation of Flexible Hidden Markov Models for Time Series of Counts. arXiv:\url{https://arxiv.org/pdf/1901.03275.pdf}.
}
\examples{
# importing example data
x = read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2
# computing the penalized negative log-likelihood
parvect = pn2pw(N=2,probs=cbind(dpois(x=0:41,lambda=14),dpois(x=0:41,lambda=26)),
gamma=matrix(c(0.95,0.05,0.05,0.95),ncol=2),delta=NULL,stationary=TRUE)
lambda = rep(10^4,2)
nLogLike(parvect=parvect,x=x,N=2,stationary=TRUE,lambda=lambda,sup=41,m=3,inflation=FALSE)
}
