psRes = function(mod){
  if(inherits(mod,"countHMM")==FALSE){
    stop("mod must be an object of type countHMM (as returned by the function fitMod).")
  }
  x = mod$x
  n = length(x)
  N = mod$N
  lalpha = matrix(NA,N,n)
  foo = mod$delta*mod$probs[c(x[1]+1),]
  sumfoo = sum(foo)
  lscale = log(sumfoo)
  foo = foo/sumfoo
  lalpha[,1] = lscale+log(foo)
  for (i in 2:n){
    foo = foo%*%mod$gamma*mod$probs[c(x[i]+1),]
    sumfoo = sum(foo)
    lscale = lscale+log(sumfoo)
    foo = foo/sumfoo
    lalpha[,i] = log(foo)+lscale
  }
  lbeta = matrix(NA,N,n)
  lbeta[,n] = rep(0,N)
  foo = rep(1/N,N)
  lscale = log(N)
  for(i in (n-1):1){
    foo = mod$gamma%*%((mod$probs[c(x[i+1]+1),])*foo)
    lbeta[,i] = log(foo)+lscale
    sumfoo = sum(foo)
    foo = foo/sumfoo
    lscale = lscale+log(sumfoo)
  }
  xc = 0:max(x)
  nxc = length(xc)
  dxc = matrix(NA,nrow=nxc,ncol=n)
  Px = matrix(NA,nrow=N,ncol=nxc)
  for(j in 1:nxc){
    Px[,j] = mod$probs[c(xc[j]+1),]
  }
  la = lalpha
  lb = lbeta
  la = cbind(log(mod$delta),la)
  lafact = apply(la,2,max)
  lbfact = apply(lb,2,max)
  for(i in 1:n){
    foo = (exp(la[,i]-lafact[i])%*%mod$gamma)*exp(lb[,i]-lbfact[i])
    foo = foo/sum(foo)
    dxc[,i] = foo%*%Px
  }
  cdists = dxc
  cumdists = rbind(rep(0,n),apply(cdists,2,cumsum))
  ulo = uhi = rep(NA,n)
  for(i in 1:n){
    ulo[i] = cumdists[x[i]+1,i]
    uhi[i] = cumdists[x[i]+2,i]
  }
  umi = 0.5*(ulo+uhi)
  npsr = qnorm(rbind(ulo,umi,uhi))
  return(npsr)
}
