littleR <- function(n=1000) {
  showVW <- function(multiplier){
    layout(cbind(1,2))
    vv = c(.7,1.2)/2
    ww = c(1,-.5)/sqrt(2)
    bb = vv + multiplier*ww
    b = v+multiplier*w
    plot( w, b, pch=20, col=rgb(0,0,0,.3),
          xlab="W",ylab="B",main=paste("Case Space: r =",signif(cor(w,b),2)))
    plot(1:10, type="n",xlim=c(-.5,2),ylim=c(-1,1.5),xlab="",
         ylab="",main=paste("Variable Space: θ=",signif(acos(cor(w,b))*180/pi,2)),xaxt="n",yaxt="n",bty="n")
    arrows(0,0,vv[1],vv[2], col="red",lwd=3,length=.1)
    arrows(0,0,ww[1],ww[2], col="black",lwd=3,length=.1)
    arrows(0,0,bb[1],bb[2], col="blue",lwd=2,length=.1,xpd=NA)
    if( multiplier!=0 )
      arrows(0,0,multiplier*ww[1],multiplier*ww[2],col=rgb(0,0,0,.3),lwd=3,length=.1,xpd=NA)
    arrows(multiplier*ww[1],multiplier*ww[2],bb[1],bb[2],col=rgb(1,0,0,.3),lwd=3,length=.1,xpd=NA)
    text(c(ww[1],bb[1]),c(ww[2],bb[2]),c("W","B"),pos=2,xpd=NA)
  }
  #
  
  if (!require(manipulate)) error("Must install manipulate package.")
  v = rnorm(n)
  w = rnorm(n)
  w = resid( lm( w~v )) 
  manipulate(showVW(multiplier),
             multiplier=slider(-5,5,initial=0,step=.1,label="W Multiplier"))
  
}

