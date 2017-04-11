#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("COMTRADE data availability"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      # Action button for updating the COMTRADDE data avilability
      actionButton(inputId = "updtCT", label = "Update COMTRADE"),
      br(),
      # sliderInput("year",
      #             "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      sliderInput("yearsRange", label = h3("Years range"), min = 2000L, max = 2020L,
                  value = c(2010L, 2017L), step = 1L),
      sliderInput("monthRange", label = h3("Month range"), min = 1L, max = 12L,
                  value = c(1L, 12L), step = 1L)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(# plotOutput("distPlot")
      tabsetPanel(
        #tabPanel("Plot", plotOutput("smmary")),
        tabPanel("Summary", verbatimTextOutput("distPlot"))
        #tabPanel("Table", tableOutput("table"))
      ))
  )
))
