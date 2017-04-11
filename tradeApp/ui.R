#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("../R/getCTPartners.R")
# source("../R/getFSR.R")
source("../R/getCTTradeClass.R")
# load("availability.rdata")
load("parts.rdata")

# Initializing all trade classification systems
classifications <- getCTTradeClass()

# Initialinsing all partners
# partners <- c(setNames(getFSR()$Partner.Code, getFSR()$Partner),setNames(getFSR()$Partner.Code, rep("FSR", 12)))

#setNames(getCTPartners()$Partner.Code, getCTPartners()$Partner)

countriesFilter <- as.list(partners[1:12])
countriesFilter$FSR <- "FSR" #as.vector(partners[13:24])

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("COMTRADE data availability"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      # Action button for updating the COMTRADDE data avilability
      # actionButton(inputId = "updtCT", label = "Update COMTRADE"),
      # br(),
      selectInput("periodicity", "Periodicity:",
                  c("Annual" = "ANNUAL",
                    "Mounthly" = "MONTHLY")),
      selectInput("merchandize", "Merchandize type:", 
                  c("Commodities" = "COMMODITIES", "Services" = "SERVICES"), selectize = TRUE),
      selectInput("system", "Coding system:", classifications, selectize = TRUE),
      selectInput("countries", "Countries:", countriesFilter, selectize = TRUE, multiple = TRUE, selected = "ALL"),
      sliderInput("yearsRange", label = h3("Years range"), min = 2000L, max = 2020L,
                  value = c(2012L, 2017L), step = 1L),
      sliderInput("monthRange", label = h3("Month range"), min = 1L, max = 12L,
                  value = c(1L, 12L), step = 1L)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(# plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Table", tableOutput("table")),
        tabPanel("Filters", verbatimTextOutput("filters"))
        
      ))
  )
))
