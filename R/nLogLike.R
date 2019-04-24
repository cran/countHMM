nLogLike = function(parvect,x,N,stationary,lambda,sup,m,inflation){
  n = length(x)
  pn = pw2pn(N=N,parvect=parvect,stationary=stationary,sup=sup)
  allprobs = matrix(1,nrow=n,ncol=N)
  ind = which(!is.na(x))
  for(i in 1:N){
    allprobs[ind,i] = pn$probs[c(x[ind]+1),i]
  }
  foo = pn$delta*allprobs[1,]
  sumfoo = sum(foo)
  lscale = log(sumfoo)
  foo = foo/sumfoo
  for(i in 2:n){
    foo = foo%*%pn$gamma*allprobs[i,]
    sumfoo = sum(foo)
    lscale = lscale+log(sumfoo)
    foo = foo/sumfoo
  }
  if(is.null(lambda)){
    lambda = rep(0,N)
  }
  penalty = 0
  for(i in 1:N){
    if(is.null(inflation)){
      penalty = penalty+lambda[i]*sum(diff(pn$probs[,i],differences=m)^2)
    }else{
      penalty = penalty+lambda[i]*sum(diff(pn$probs[-c(inflation+1),i],differences=m)^2)
    }
  }
  return(-lscale+penalty)
}
