library(shiny)
passwords <- read.csv("passwords.csv")
responses <- read.csv("responses.csv")

# Function to handle a submission
doSubmit <- function(prob="default",item="default",user="nobody",value=-1) {
  # assemble a line of the <responses> file
  newSubmit <- data.frame(UserID=user,File=prob,Item=item,Response=value,Answer=value,TimeStamp=date())
  # Rediculously inefficient! Just a proof that it can be done
  # Switch to SQLlite of something similar
  responses <<- rbind(responses, newSubmit) # in global environment
  write.csv(responses, "responses.csv", row.names=FALSE)
}


# Define server logic
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  
  #probName <- reactive({input$ThisProb})
#  fileName <- reactive({paste('Contents/',probName(),".html",sep="")})()
#  hoo <- readChar(fileName, file.info(fileName)$size)
 # hoo <- paste(hoo,hoo)
  output$probContents <- renderText({
    fileName <- paste("Contents/",input$ThisProb, ".html",sep="")
    hoo <- readChar(fileName, file.info(fileName)$size)
    # HTML(paste(getwd()))
    # or try includeHTML() or includeMarkdown()
    HTML(hoo)
    })
  output$tout1 <- renderPrint({cat(input$text1)})
  output$out1 <- renderPrint({doSubmit(prob=isolate(input$ThisProb),
                                       item=1,
                                       user=isolate(input$UserID),
                                       value=input$in1); 
                              cat(input$in1)})
  # Just repeated for all the other inputs
  output$out2 <- renderPrint({doSubmit(prob=isolate(input$ThisProb),
                                       item=2,
                                       user=isolate(input$UserID),
                                       value=input$in2); 
                              cat(input$in2)})
  
  output$out3 <- renderPrint({doSubmit(prob=isolate(input$ThisProb),
                                       item=3,
                                       user=isolate(input$UserID),
                                       value=input$in3); 
                              cat(input$in3)})
  
  output$out4 <- renderPrint({doSubmit(prob=isolate(input$ThisProb),
                                       item=4,
                                       user=isolate(input$UserID),
                                       value=input$in4); 
                              cat(input$in4)})
  
})