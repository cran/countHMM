stateDec = function(mod){
  if(inherits(mod,"countHMM")==FALSE){
    stop("mod must be an object of type countHMM (as returned by the function fitMod).")
  }
  x = mod$x
  n = length(x)
  xi = matrix(0,n,mod$N)
  foo = mod$delta*mod$probs[c(x[1]+1),]
  xi[1,] = foo/sum(foo)
  for(i in 2:n){
    foo = apply(xi[i-1,]*mod$gamma,2,max)*mod$probs[c(x[i]+1),]
    xi[i,] = foo/sum(foo)
  }
  iv = numeric(n)
  iv[n] = which.max(xi[n,])
  for(i in (n-1):1){
    iv[i] = which.max(mod$gamma[,iv[i+1]]*xi[i,])
  }
  return(iv)
}
