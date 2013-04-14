library(shiny)

# Find the problems in the directory
# TO DO: Make pull out the assignment directories, then have problems within those
possibleProblems <- dir("Contents")
htmlFiles <- possibleProblems[grep(".+\\.html",possibleProblems)]
htmlNames <- sub(".html","",htmlFiles)

# Maybe also look for different filenames, 
# -finished == don't accept answers
# -showanswers == show the answers
# -interactive == display the answers interactively


# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Math 155 Exercises"),
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    textInput("UserID","User ID:"),
    textInput("Passwd","Password:"),
    selectInput("ThisProb","Assignment:",htmlNames)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    htmlOutput("probContents")
  )
))