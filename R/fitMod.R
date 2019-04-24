fitMod = function(x,N=2,probs0=NULL,gamma0=NULL,delta0=NULL,stationary=TRUE,lambda=NULL,sup=NULL,m=3,inflation=NULL){
  if(is.null(sup)){
    sup = max(x)
  }
  if(is.null(probs0)){
    probs0 = NULL
    for(i in 1:N){
      probs0 = cbind(probs0,dpois(0:sup,lambda=as.numeric(quantile(x,probs=seq(0.25,0.75,length=N)[i]))))
    }
  }
  if(sup!=(nrow(probs0)-1)){
    stop(paste("probs0 must be a matrix with",sup+1,"rows and",N,"columns."))
  }
  if(is.null(gamma0)){
    gamma0 = matrix(0.1/(N-1),nrow=N,ncol=N)
    diag(gamma0) = 0.9
  }
  if(nrow(gamma0)!=N|ncol(gamma0)!=N){
    stop(paste("gamma0 must be a matrix with",N,"rows and",N,"columns."))
  }
  if(!is.null(delta0)&length(delta0)!=N){
    stop(paste("delta0 must be a vector of length",N,"."))
  }
  parvect0 = pn2pw(N=N,probs=probs0,gamma=gamma0,delta=delta0,stationary=stationary)
  mod = nlm(nLogLike,parvect0,x=x,N=N,stationary=stationary,lambda=lambda,sup=sup,m=m,inflation=inflation)
  pn = pw2pn(N=N,mod$estimate,stationary=stationary,sup=sup)
  mllk = mod$minimum
  mod = list(N=N,probs=pn$probs,gamma=pn$gamma,delta=pn$delta,code=mod$code,mllk=mllk,estimate=mod$estimate,x=x)
  class(mod) = append("countHMM",class(mod))
  return(mod)
}
