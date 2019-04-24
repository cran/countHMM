plotMod = function(mod){
  if(inherits(mod,"countHMM")==FALSE){
    stop("mod must be an object of type countHMM (as returned by the function fitMod).")
  }
  if(mod$N>7){
    stop("plotMod does not support N>7 yet.")
  }
  cols = c("#2166ac","#b2182b","#56b4e9","#e69f00","#d9f0a3","#005a32","#41ab5d")
  delta = solve(t(diag(mod$N)-mod$gamma+1),rep(1,mod$N))
  mar = rep(0,nrow(mod$probs))
  for(i in 1:mod$N){
    mar = mar+delta[i]*mod$probs[,i]
  }
  par(mfrow=c(1,1))
  plot(x=0:(nrow(mod$probs)-1),y=as.numeric(table(factor(mod$x,levels=0:(nrow(mod$probs)-1))))/length(mod$x),ylim=c(0,max(as.numeric(table(factor(mod$x,levels=0:(nrow(mod$probs)-1))))/length(mod$x))*1.025),xlab="counts",ylab="probability",main="state-dependent distributions",type="h",col="lightgray")
  for(i in 1:mod$N){
    lines(x=0:(nrow(mod$probs)-1),y=delta[i]*mod$probs[,i],type="p",col=cols[i])
  }
  lines(x=0:(nrow(mod$probs)-1),y=mar,type="l",lty=2)
}
