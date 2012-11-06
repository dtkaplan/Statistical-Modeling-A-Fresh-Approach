library(shiny)

# Use Shiny for a clicker-like application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Election 2012 Prediction"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    helpText("Relative Weights:"),
    sliderInput("silver", "Silver's Prediction Weight:",
                min=0, max=1, value=.25,step=0.01),
    sliderInput("norm1",
                "Norm1 Weight:",
                min=0, max=1, value=.25,step=0.01),
    sliderInput("norm2", "Norm2 Weight :",
                min=0, max=1, value=.25,step=0.01),
    sliderInput("uniform", "Uniform Weight:",
                min=0, max=1, value=.25,step=0.01),
    helpText("Norm 1 Params:"),
    numericInput("mn1", "Mean 1:", value=280, min=1,max=538),
    numericInput("sd1", "SD 1", value=10, min=2,max=200),
    helpText("Norm 2 Params:"),
    numericInput("mn2", "Mean 2", value=260, min=2,max=538),
    numericInput("sd2", "SD 2", value=10, min=1,max=200),
    helpText("Uniform Params:"),
    numericInput("umin", "Min", value=210, min=0,max=537),
    numericInput("umax", "Max:", value=310, min=1,max=538)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    numericInput("outcome", HTML("<span style='color:red;'>Electoral Vote Outcome</span>"),
                min=0, max=538, value=269),
    h4(textOutput("summary")),
    plotOutput("electors")
  )
))
