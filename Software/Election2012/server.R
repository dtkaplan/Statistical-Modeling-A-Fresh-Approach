library(shiny)
# starting with 196.  The third 1 is at 211.  The 4 is at 270
silverdist = c(
  1,0,0,1,             # 200
  0,0,0,0,0,0,0,0,0,0, # 210
  1,0,0,0,1,2,1,0,0,1, # 220
  0,0,0,1,0,0,0,1,0,0, # 230
  1,1,1,1,0,1,1,0,0,0, # 240
  3,2,0,1,1,1,2,2,2,0, # 250
  1,1,1,3,5,1,1,1,1,4, # 260
  1,3,1,1,1,4,3,1,2,4, # 270
  9,2,2,0,1,2,8,3,8,8, # 280
  1,0,2,2,18,0,1,5,2,3, #290
  0,4,1,20,0,2,2,19,0,1,#300
  2,5,1,4,1,0,1,2,75,5, #310
  2,2,1,0,2,1,0,1,1,0,  #320
  1,16,4,1,1,4,1,2,1,0, #330
  2,89,19,1,1,0,1,0,0,1,0,#340
  1,1,0,0,57,25,0,2,1,0, #350
  1,0,0,1,1,2,2,0,1,1)
silverdist = silverdist/sum(silverdist)
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  votes <- 0:538
  inds <- 196 + (0:(length(silverdist)-1))
  prob <- prob2 <- prob3 <- weightsum <- 0
  findProb = reactive(function() {
    res <- rep(0,539)
    weightsum <<- input$silver + input$norm1 + input$norm2 + input$uniform
    prob1 <<- dnorm(votes, mean=input$mn1, sd=input$sd1)*input$norm1/weightsum 
    prob2 <<- dnorm(votes, mean=input$mn2, sd=input$sd2)*input$norm2/weightsum
    prob3 <<- dunif(votes, min=input$umin, max=input$umax)*input$uniform/weightsum
    res <- prob1+prob2+prob3

    res[inds+1] <- res[inds+1]+silverdist*input$silver/weightsum
    return(res)
  })
    # all zeros, index shifted by 1 since it starts at votes=0
  output$electors <- reactivePlot(function() {
    prob <- findProb()
    plot( prob~votes, type="l", xlab="Electors", ylab="Probability", xlim=c(200,400))
    points(inds, silverdist*input$silver/weightsum,pch=20,cex=.5,type="h")
    polygon( votes, prob, col=rgb(0,0,0,.2)) #gray
    polygon( votes, prob1, col=rgb(0,0,1,.20)) # blue
    polygon( votes, prob2, col=rgb(0,1,0,.20)) # green
    polygon( votes, prob3, col=rgb(157/255,1,212/255,.20)) # aquarmarine
    lines(input$outcome*c(1,1), c(0,1), col=rgb(1,0,0,.3), lwd=3)
  })
  output$summary <- reactiveText(function() {
    prob <- findProb()
    return(paste("prob(outcome=",input$outcome," | model) = ",
          signif(prob[input$outcome+1],3), sep=""))
  })

})
