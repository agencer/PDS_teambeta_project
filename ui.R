library(shiny)  ## Load shiny

statecodes <- read.csv("https://raw.githubusercontent.com/williamloh1/teambeta/master/Project/datasets/state%20codes.csv")

shinyUI(fluidPage( ## Flexible layout function 
  titlePanel("COVID-19 Search Trends"), ## Setting the title
  sidebarLayout( ## Choosing layout with inputs on side and 
    ## outputs displayed in the main body
    sidebarPanel( #Things in this function specify the sidebar
      selectInput(label = "State: ",
                  choices = statecodes$state, inputId = "stateterm"),
      textInput(inputId = "searchterm", 
                label = "Search Term:",
                value=""
      ), 
      selectInput(label = "Type of query: ",
                  choices = c("Hits per Day","Day-to-day change in hits"),
                  inputId = "querytype")),
      ## End of sidebar
    mainPanel( ## Arguments for main section (output)
      plotOutput("searchGraph")
    ) # Close main panel
  ) #
))