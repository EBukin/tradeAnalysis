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
      selectInput("periodicity", "Periodicity:", c("ANNUAL", "MONTHLY"), selected = "ANNUAL"),
      uiOutput("uiCountries"),
      uiOutput("uiYear"),
      # checkboxInput("HS2", "HS 2 digits", TRUE, width = NULL),
      checkboxGroupInput("HSdigits", "Number of HS digits", 
                         choices = c("2-digits" = 2L,  
                                     "4-digits" = 4L, 
                                     "6-digits" = 6L), 
                         selected = c(2L, 4L, 6L), inline = TRUE)
      #actionButton(inputId = "updtCT", label = "Update COMTRADE")
    ),
    
    # Show actions in the tabs
    mainPanel(# plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Table",
                 tableOutput("table")),
        tabPanel("Filters", verbatimTextOutput("filters"))
        
      ))
  )
))
