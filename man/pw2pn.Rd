\name{pw2pn}
\alias{pw2pn}
\title{pw2pn}
\usage{
pw2pn(N,parvect,stationary,sup)
}
\arguments{
\item{N}{Integer, number of states.}
\item{parvect}{Vector of working parameters.}
\item{stationary}{Logical, determines whether the initial distribution of the Markov chain underlying the observed counts is the stationary distribution.}
\item{sup}{Integer, determines the upper bound of the support of the state-dependent distributions.}
}
\value{
A list of natural parameters.
}
\description{
Transforming working parameters to natural parameters. Not intended to be run by the user (internal function, called by the functions \code{\link{nLogLike}} and \code{\link{fitMod}}).
}
\examples{
# transforming natural parameters to working paramters
parvect = pn2pw(N=2,probs=cbind(dpois(x=0:41,lambda=14),dpois(x=0:41,lambda=26)),
gamma=matrix(c(0.95,0.05,0.05,0.95),ncol=2),delta=NULL,stationary=TRUE)
# transforming working parameters to natural parameters
pw2pn(N=2,parvect=parvect,stationary=TRUE,sup=41)
}
