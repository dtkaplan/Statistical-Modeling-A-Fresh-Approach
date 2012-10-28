library(shiny)
library(datasets)

.libPaths('~/R/library')

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  newRandomDir <- reactive(function(){
    x <- input$newvector # just trigger the function
    ang = runif(1,min=0,max=2*pi)
    r = rnorm(1)
    c(r*cos(ang),r*sin(ang),sign(rnorm(1)))
   })
  newRandomVec <- reactive(function(){
    input$res.sd*newRandomDir()*c(c(1,1)/sqrt(input$n),sqrt(1-1/input$n)) # in the deterministic plane
  })
    
  # Generate the plot
  output$ciPlot <- reactivePlot(function() {
   drawSamplingGeometry(bcoef=input$bcoef,ccoef=input$ccoef,
                        theta=input$theta,res.sd=input$res.sd,
                        n=input$n,
                        bunits=input$blength,cunits=input$clength,
                        showBnaive=input$showBnaive,
                        showBcontours=input$showBcontours,
                        showCcontours=input$showCcontours,
                        showRandom=FALSE,
                        noise = newRandomVec()
                        )
  })
  output$ciMeasPlot <-  reactivePlot(function() {
   drawSamplingGeometry(bcoef=input$bcoef,ccoef=input$ccoef,
                        theta=input$theta,res.sd=input$res.sd,
                        n=input$n,
                        bunits=input$blength,cunits=input$clength,
                        showBnaive=input$showBnaive,
                        showBcontours=input$showBcontours,
                        showCcontours=input$showCcontours,
                        showRandom=TRUE,
                        noise = newRandomVec(),
                        showSample=TRUE,
                        showSamplingDist=FALSE
                        )
  })
  output$ci3D <- reactivePlot(function(){
    if( require(scatterplot3d,quietly=TRUE)) {
      draw3dview(bcoef=input$bcoef, ccoef=input$ccoef, theta=input$theta,
               res.sd=input$res.sd, n=input$n,noise=newRandomVec(),
                 showResamps=input$showSimulation)
    } else  stop("Must install scatterplot3d package")
  })
    
  output$ciDetermPlot <-  reactivePlot(function() {
   drawSamplingGeometry(bcoef=input$bcoef,ccoef=input$ccoef,
                        theta=input$theta,res.sd=input$res.sd,
                        n=input$n,
                        bunits=1, cunits=1, # Always show in natural units
                        #bunits=input$blength,cunits=input$clength,
                        showBnaive=input$showBnaive,
                        showBcontours=input$showBcontours,
                        showCcontours=input$showCcontours,
                        showRandom=FALSE,
                        noise = newRandomVec(),
                        showSample=FALSE,
                        showSamplingDist=FALSE
                        )
  })
})

# Drawing Program =================================
drawSamplingGeometry <- function(bcoef=1.5, ccoef=NULL, theta=90, res.sd=2, n=10,
                                 bunits=1,cunits=1, showBnaive=TRUE,
                                 showBcontours=TRUE, showCcontours=FALSE,
                                 showRandom=TRUE,
                                 showSamplingDist=TRUE,
                                 showSample=TRUE,
                                 noise=c(.1,-.2) ){
  bunits=as.numeric(bunits); cunits=as.numeric(cunits)
  bvec = c(1.3,.5)*bunits
  # absolute direction of covariate vector
  cang = atan2(bvec[2],bvec[1]) + theta*pi/180 
  cvec = c(1.5*cos(cang),1.5*sin(cang))*cunits
  par( bty="n") # no box
  plot( 1:2, xlim=c(-5,5),ylim=c(-5,5),type="n",xaxt="n",yaxt="n",
        xlab="",ylab="")
  arrows(0,0,bvec[1],bvec[2],lwd=3, col="blue",length=.1)
  lines( c(-10,10)*bvec[1], c(-10,10)*bvec[2], col="blue", lty=2 )
  lines( c(-10,10)*cvec[1], c(-10,10)*cvec[2], col="green", lty=2 )
  if(!is.null(ccoef)) arrows(0,0,cvec[1],cvec[2],col="green",lwd=3,length=.1)
  # Draw the deterministic value
  Apos = bcoef*bvec/bunits
  if(!is.null(ccoef)) Apos = Apos + ccoef*cvec/cunits
  points(Apos[1],Apos[2],pch=3,col="black") # The deterministic value
  text(Apos[1],Apos[2],"A",pos=4)

  if( showBnaive ) { # Draw the bcoef contours without accounting for the covariate
     direction = atan2(bvec[2],bvec[1]) + pi/2 # perpendicular to B
     for(k in -40:40) {
      lines( c(-1000,1000)*cos(direction)+k*bvec[1], 
             c(-1000,1000)*sin(direction)+k*bvec[2],lty=3,col="blue")
      position = k*bvec + 2*c(cos(direction),sin(direction))
      text( position[1],position[2],k,col="blue")
    }
  }
  
  if( showBcontours ) {   # Draw the bcoef coordinate contours
    if(is.null(ccoef)) {# no C vector
      direction = atan2(bvec[2],bvec[1]) + pi/2 # perpendicular to B
      cvec = c(sin(direction),cos(direction)) # unit perpendicular to B
    }
    else  direction=cang  # along C
    for(k in -40:40) {
      lines( c(-1000,1000)*cos(direction)+k*bvec[1], 
             c(-1000,1000)*sin(direction)+k*bvec[2],lty=1,col="gray")
      position = k*bvec - 2*cvec
      text( position[1],position[2],k,col="gray")
    }
  }
    if( showCcontours ) {   # Draw the ccoef coordinate contours
    if(!is.null(ccoef)) {# no C vector
      direction=atan2(bvec[2],bvec[1])  # along B
      for(k in -40:40) {
         lines( c(-1000,1000)*cos(direction)+k*cvec[1], 
             c(-1000,1000)*sin(direction)+k*cvec[2],lty=1,col="green")
         position = k*cvec - 2*bvec
         text( position[1],position[2],k,col="green")
      }
    }
  }
  # draw the sample value
 
  Ahat = Apos + noise[c(1,2)]
  if( showRandom) arrows(Apos[1],Apos[2],Ahat[1],Ahat[2],col=rgb(1,0,0,.5),lwd=5,length=.1)
  if( showSample ) {
    points(Ahat[1],Ahat[2],pch=20,col="black")
    points(Ahat[1],Ahat[2],pch=1,cex=2,col="black")
  }
  if( showSamplingDist ) {# draw the residual radius
    angs = seq(0,2*pi,length=100)
    xpts = Ahat[1] + res.sd*cos(angs)
    ypts = Ahat[2] + res.sd*sin(angs)
    polygon(xpts,ypts,col=rgb(1,0,0,.05),border=NA)
    # draw more density where the random vectors are likely to be
    for (scaling in c(1.96,1.33,1,.7,.44)){
      xpts = Ahat[1] + scaling*res.sd*cos(angs)/sqrt(n)
      ypts = Ahat[2] + scaling*res.sd*sin(angs)/sqrt(n)
      polygon(xpts,ypts,col=rgb(1,0,0,.2),border=NA)
  }
  }
}
# ========
draw3dview <- function(bcoef=1.5, ccoef=2, theta=90, res.sd=2, n=10,noise=rep(1,3),showResamps=TRUE) {
  bvec = c(1.3,.5)
  # absolute direction of covariate vector
  cang = atan2(bvec[2],bvec[1]) + theta*pi/180 
  cvec = c(1.5*cos(cang),1.5*sin(cang))
  par( bty="n") # no box
  z = seq(-1,3,length=100)
  x = cos(z)
  y = sin(z)
  pframe = scatterplot3d(x,y,z,main="3-D View",xlab="",ylab="", grid=FALSE,
                axis=FALSE,zlim=c(-3,5),xlim=c(-5,5),ylim=c(-5,5),type="n")
  pframe$plane3d(0,0,0, lty=1,col="gray")
  pframe$points3d(c(0,bvec[1]),c(0,bvec[2]),c(0,0),type="l",lwd=3, col="blue")
  pframe$points3d(c(-3,3)*bvec[1],c(-3,3)*bvec[2], c(0,0),
                  type="l", col="blue",lty=2)
  pframe$points3d(c(0,cvec[1]),c(0,cvec[2]),c(0,0),type="l",lwd=3, col="green")
  pframe$points3d(c(-3,3)*cvec[1],c(-3,3)*cvec[2], c(0,0),
                  type="l", col="green",lty=2)
  Adet = bcoef*c(bvec,0) + ccoef*c(cvec,0)
  Apos = Adet + noise
  projectColor = ifelse(Apos[3]>0, rgb(.3,.3,.3),rgb(.3,.3,.3,.4))
  pframe$points3d(c(1,1)*Apos[1],c(1,1)*Apos[2],c(0,Apos[3]),col=projectColor,lwd=3,type="l")
  pframe$points3d(Apos[1],Apos[2],Apos[3],pch=20,col="black")
  pframe$points3d(Apos[1],Apos[2],0,pch=1,cex=1,col="black")
  resid.color = rgb(1,0,0,ifelse( Apos[3]>0,1,.3))
  pframe$points3d(c(Adet[1],Apos[1]),c(Adet[2],Apos[2]), c(0,Apos[3]), 
                  col="red", lwd=3, type="l")
  pframe$points3d(Adet[1],Adet[2],0,pch=4,cex=1,col="black")

  # Show the resampling variation
  if( showResamps ){
    for (k in 1:50) {
      newx = Apos[1] + rnorm(1)*sqrt(1/n)*res.sd/sqrt(3)
      newy = Apos[2] + rnorm(1)*sqrt(1/n)*res.sd/sqrt(3)
      newz = Apos[3] + rnorm(1)*res.sd/sqrt(3)
      pframe$points3d(newx,newy,newz,
                    col=rgb(1,0,0,.2), pch=20)
      pframe$points3d(newx, newy, 0,
                    col=rgb(1,0,0,.4), pch=1)
      pframe$points3d(c(Apos[1], newx),
                    c(Apos[2], newy),
                    c(Apos[3], newz),
                    col=rgb(1,0,0,.2), type="l")
      pframe$points3d(c(newx, newx),
                    c(newy, newy),
                    c(0, newz),
                    col=rgb(0,0,0,.2), type="l")
    }
  }
    
}
