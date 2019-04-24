plotRes = function(mod){
  if(inherits(mod,"countHMM")==FALSE){
    stop("mod must be an object of type countHMM (as returned by the function fitMod).")
  }
  if(mod$N>7){
    stop("plotRes does not support N>7 yet.")
  }
  cols = c("#2166ac","#b2182b","#56b4e9","#e69f00","#d9f0a3","#005a32","#41ab5d")
  states = stateDec(mod)
  colvec = rep(cols[1],length(mod$x))
  for(i in 2:mod$N){
    colvec[states==i] = cols[i]
  }
  res = psRes(mod)
  par(mfrow=c(1,2))
  qqnorm(res[2,],xlab="theoretical quantiles",ylab="sample quantiles",main="quantile-quantile plot",col=colvec)
  qqline(res[2,],col="red")
  acf(res[2,],xlab="lag",ylab="autocorrelation",main="autocorrelation")
}
