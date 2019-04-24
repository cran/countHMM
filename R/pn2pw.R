pn2pw = function(N,probs,gamma,delta,stationary){
  tprobs = c()
  for(i in 1:N){
    tprobs = c(tprobs,log(probs[-1,i]/probs[1,i]))
  }
  foo = log(gamma/diag(gamma))
  tgamma = as.vector(foo[!diag(N)])
  if(stationary){
    tdelta = NULL
  }else{
    tdelta = log(delta[-1]/delta[1])
  }
  parvect = c(tprobs,tgamma,tdelta)
  return(parvect)
}
