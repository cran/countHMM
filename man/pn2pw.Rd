\name{pn2pw}
\alias{pn2pw}
\title{pn2pw}
\usage{
pn2pw(N,probs,gamma,delta,stationary)
}
\arguments{
\item{N}{Number of states.}
\item{probs}{Matrix with \code{N} columns which contains the state-dependent probabilities for each count.}
\item{gamma}{Transition probability matrix of the Markov chain underlying the observed counts with \code{N} rows and \code{N} columns.}
\item{delta}{Initial distribution vector of length \code{N} of the Markov chain underlying the observed counts. If \code{NULL}, then the stationary distribution is returned.}
\item{stationary}{Logical, determines whether the initial distribution of the Markov chain underlying the observed counts is the stationary distribution.}
}
\value{
A vector of working parameters.
}
\description{
Transforming natural parameters to working parameters. Not intended to be run by the user (internal function, called by the functions \code{\link{nLogLike}} and \code{\link{fitMod}}).
}
\examples{
# transforming natural parameters to working paramters
pn2pw(N=2,probs=cbind(dpois(x=0:41,lambda=14),dpois(x=0:41,lambda=26)),
gamma=matrix(c(0.95,0.05,0.05,0.95),ncol=2),delta=NULL,stationary=TRUE)
}
