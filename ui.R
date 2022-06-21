library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
# Use a fluid Bootstrap layout
fluidPage(theme = shinytheme("united"),    
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  # Give the page a title
  titlePanel("Expected Rate of Return"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      helpText("Factor data provided by Ken French and Return data provided by Yahoo Finance"),
      
      selectInput("Frequency", "Frequency:", 
                  choices=c("Daily","Monthly")),
      
      selectInput("Model", "Number or Factors:", 
                  choices=c("5 factor model","3 factor model")),
                  
      hr(),
                  
      
      textInput("ticker", "Stock Ticker:", "FB")
      ),
  

    mainPanel(
      tabsetPanel(
        tabPanel("Tab 1",
          verbatimTextOutput("Rates"),
          tableOutput("earningEstimates"),
          tableOutput("growthEst")
        ),
        tabPanel("Tab 2",
          tableOutput("Ttest"),
          helpText("Standard errors are heteroskedastic robust")
        )
      )
    )
    
  )
)

