library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Geometry of Confidence Intervals"),
  sidebarPanel(
    helpText("Deterministic Part of System: vector A"),
    numericInput("bcoef", "Coef. on B (med. units)", value=2,min=-2.5,max=2.5,step=.25),
    numericInput("ccoef", "Coef. on C (med. units)", value=0,min=-3,max=3,step=.25),
    sliderInput("theta","Angle of colinearity:", min=0,max=180,value=90),
    helpText("Random Part of Model"),           
    sliderInput("res.sd", "Standard Deviation of Noise:", min=.1,max=4,value=1,step=.1),
    checkboxInput("newvector", "Generate new noise.",value=FALSE),
    helpText("Data Collection"),    
    numericInput("n", "Number of Cases:", value=10, min=2,max=10000),
    selectInput("blength","Units of B measurement:", list(Med=1,Small=.5,Big=1.7)),
    selectInput("clength","Units of C measurement:", list(Med=1,Small=.5,Big=1.7)),
    helpText("Display Choices"),
    checkboxInput("showBcontours", "Show B coef contours", FALSE ),
    checkboxInput("showCcontours", "Show C coef contours", FALSE ),
    checkboxInput("showRandom", 'Show measurement "noise"', TRUE )
  ),
                          # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),

    plotOutput("ciPlot",width="500px",height="500px")
  )
))
