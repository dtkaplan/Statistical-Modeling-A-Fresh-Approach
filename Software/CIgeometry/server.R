library(shiny)
library(datasets)

#resid.ang = rnorm(1,mean=pi/2,sd=pi/6)*sign(rnorm(1)) #Should stay the same except for new data

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  drawNewAngle <- reactive(function(){
    x <- input$newvector # just trigger the function
     rnorm(1,mean=pi/2,sd=pi/6)*sign(rnorm(1))
   })
  # Generate the plot 
  output$ciPlot <- reactivePlot(function() {
   drawSamplingGeometry(bcoef=input$bcoef,ccoef=input$ccoef,
                        theta=input$theta,res.sd=input$res.sd,
                        n=input$n,
                        bunits=input$blength,cunits=input$clength,
                        showBcontours=input$showBcontours,
                        showCcontours=input$showCcontours,
                        showRandom=input$showRandom,
                        resid.ang = drawNewAngle()
                        )
  })
})

# Drawing Program =================================
drawSamplingGeometry <- function(bcoef=1.5, ccoef=NULL, theta=90, res.sd=2, n=10,
                                 bunits=1,cunits=1,
                                 showBcontours=TRUE, showCcontours=FALSE,
                                 showRandom=TRUE,
                                 resid.ang=pi/2.3) {
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
  if(!is.null(ccoef)) arrows(0,0,cvec[1],cvec[2],col="green",lwd=3,length=.1)
  # Draw the deterministic value
  Apos = bcoef*bvec/bunits
  if(!is.null(ccoef)) Apos = Apos + ccoef*cvec/cunits
  points(Apos[1],Apos[2],pch=20,col="black")
  text(Apos[1],Apos[2],"A",pos=4)

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
 
  Ahat = Apos + res.sd*c(cos(resid.ang),sin(resid.ang))
  if( showRandom) arrows(Apos[1],Apos[2],Ahat[1],Ahat[2],col=rgb(1,0,0,.5),lwd=5,length=.1)
  points(Ahat[1],Ahat[2],pch=20,col="black")
  text(Ahat[1],Ahat[2],expression(hat(A)),pos=ifelse(Ahat[2]>0,3,1))
  # draw the residual radius
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
