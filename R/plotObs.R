plotObs = function(mod){
  if(inherits(mod,"countHMM")==FALSE){
    stop("mod must be an object of type countHMM (as returned by the function fitMod).")
  }
  if(mod$N>7){
    stop("plotObs does not support N>7 yet.")
  }
  cols = c("#2166ac","#b2182b","#56b4e9","#e69f00","#d9f0a3","#005a32","#41ab5d")
  states = stateDec(mod)
  par(mfrow=c(1,1))
  plot(x=c(1:length(mod$x))[states==1],mod$x[states==1],ylim=c(min(mod$x)*0.975,max(mod$x)*1.025),xlab="time",ylab="counts",main="Viterbi-decoded time series",col=cols[1])
  for(i in 2:mod$N){
    lines(x=c(1:length(mod$x))[states==i],mod$x[states==i],type="p",col=cols[i])
  }
}
