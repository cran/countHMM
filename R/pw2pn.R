pw2pn = function(N,parvect,stationary,sup){
  probs = matrix(NA,nrow=sup+1,ncol=N)
  for(i in 1:N){
    foo = c(1,exp(parvect[((i-1)*(sup)+1):(i*(sup))]))
    probs[,i] = foo/sum(foo)
  }
  gamma = diag(N)
  gamma[!gamma] = exp(parvect[(N*(sup)+1):(N*(sup)+N*N-N)])
  gamma = gamma/apply(gamma,1,sum)
  if(stationary==TRUE){
    delta = solve(t(diag(N)-gamma+1),rep(1,N))
  }else{
    foo = c(1,exp(parvect[(N*(sup)+N*N-N+1):(N*(sup)+N*N-1)]))
    delta = foo/sum(foo)
  }
  return(list(probs=probs,gamma=gamma,delta=delta))
}
