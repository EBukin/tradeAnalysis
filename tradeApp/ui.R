#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Initializing all trade classification systems
# classifications <- getCTTradeClass()
# 
# # Initialinsing all partners
# # partners <- c(setNames(getFSR()$Partner.Code, getFSR()$Partner),setNames(getFSR()$Partner.Code, rep("FSR", 12)))
# 
# #setNames(getCTPartners()$Partner.Code, getCTPartners()$Partner)
# 
# countriesFilter <- as.list(partners[1:12])
# countriesFilter$FSR <- "FSR" #as.vector(partners[13:24])

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("COMTRADE data availability"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("uiCountries"),
      uiOutput("uiPeriodicity"),
      uiOutput("uiCoding"),
      uiOutput("uiYear")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(# plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Table", tableOutput("table")),
        tabPanel("Filters", verbatimTextOutput("filters"))
        
      ))
  )
))
