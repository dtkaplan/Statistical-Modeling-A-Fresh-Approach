library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
 
  # Application title
  headerPanel("Geometry of Covariation and Sampling"),
  sidebarPanel(
    helpText("Factors to Control"),
    numericInput("n", "Measurement - Sample Size n:", value=10, min=2,max=10000),
    sliderInput("res.sd", "Standard Deviation of Noise:", min=.1,max=4,value=2.5,step=.1),   
    sliderInput("theta","Deterministic: Angle of Colinearity:", min=2,max=178,value=90),     
    helpText("Model A ~ B"),
    checkboxInput("showBnaive", "B coef contours", FALSE ),
    helpText("Model A ~ B + C"), 
    checkboxInput("showBcontours", "B coef contours in A ~ B + C", FALSE ),
    checkboxInput("showCcontours", "C coef contours in A ~ B + C", FALSE ),
    div(HTML("Audio introduction")),
    div(HTML('<iframe width="250" height="40" src="http://www.youtube.com/embed/yDPfEaY-j9E?rel=0&autohide=0&modestbranding=1" frameborder="0"></iframe>'))            
  ),
  mainPanel(
     tabsetPanel(
        tabPanel("Deterministic",        
          helpText("Deterministic Part of System: vector A."),
          div(HTML('Narration: <iframe width="300" height="40" src="http://www.youtube.com/embed/FPjjFAFBHaQ?rel=0&autohide=0&modestbranding=1" frameborder="0"></iframe>')),
          numericInput("bcoef", HTML("<span style='color:blue;'>Coef. on B</span>"), value=2,min=-2.5,max=2.5,step=.25),
          numericInput("ccoef", HTML("<span style='color:green;'>Coef. on C</span>"), value=0,min=-3,max=3,step=.25),
          plotOutput("ciDetermPlot",width="500px",height="500px")
        ),
        tabPanel("Measurement",
          div(HTML('Narration: <iframe width="300" height="40" src="http://www.youtube.com/embed/HNcSinU1HTU?rel=0&autohide=0&modestbranding=1" frameborder="0" allowfullscreen></iframe>')),
          checkboxInput("newvector", HTML("<span style='color:red'>Generate new sample.</span>"),value=FALSE), 
          selectInput("blength","Units of B measurement:", list(Med=1,Small=.5,Big=1.7)),
          selectInput("clength","Units of C measurement:", list(Med=1,Small=.5,Bg=1.7)),
          plotOutput("ciMeasPlot",width="500px",height="500px")
        ),
        tabPanel("3-D View",
           div(HTML('Narration: <iframe width="300" height="40" src="http://www.youtube.com/embed/b06-G799Zmc?rel=0&autohide=0&modestbranding=1" frameborder="0" allowfullscreen></iframe>')),
           checkboxInput("newvector", HTML("<span style='color:red'>Generate new sample.</span>"),value=FALSE),
          checkboxInput("showSimulation", "Show CI simulation", value=FALSE),
          plotOutput("ci3D", width="500px",height="500px")
        ),
        tabPanel("Conf. Interval",
          div(HTML('Narration: <iframe width="300" height="40" src="http://www.youtube.com/embed/9aalyMDN2j4?rel=0&autohide=0&modestbranding=1" frameborder="0"></iframe>')),
          checkboxInput("newvector", HTML("<span style='color:red'>Generate new sample.</span>"),value=FALSE),
          plotOutput("ciPlot",width="500px",height="500px")
        )
     )
   )        
))
