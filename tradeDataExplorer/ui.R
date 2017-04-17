#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("COMTRADE data availability"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("periodicity", "Periodicity", c("ANNUAL", "MONTHLY"), selected = "ANNUAL", inline = TRUE),
      selectInput("reporters","Reporter", 
                  setNames(tradeAnalysis::getFSR()$Partner.Code, tradeAnalysis::getFSR()$Partner),
                  selectize = TRUE, multiple = FALSE,  selected =  c(804) ),
      sliderInput("yearsRange", label = "Years range", min = 2000, max = 2020, value = c(2010, 2017), step = 1L),
      radioButtons("dataType", "Variable", c("Trade value" = "Trade.Value..US..", "Quantity kg" = "Netweight..kg."), selected = "Trade.Value..US..", inline = TRUE),
      numericInput("NPartners", label = "Number of top partners to show", value = 5),
      numericInput("NPeriods", label = "Number of periods used for top partners", value = 3),
      checkboxInput("oneEUCB", "Use EU as one region", value = TRUE),
      checkboxInput("oneFSRCB", "Use FSR as one region"),
      checkboxInput("oneRUSCB", "Show Russia separately", value = TRUE)
    ),
    
    # Show actions in the tabs
    mainPanel(# plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Top partners",
                 radioButtons("PartnersHSTypes", 
                              "Types of HS codes to present",
                              choices = c("WTO AgriFood" = "Aggs",
                                          "Main components of WTO AgriFood" = "MainAgFood",
                                          "Relevant to WTO AgriFood 2-6 digits codes" = "RelevantAgFood",
                                          "All available" = "All"),
                              selected = c("Aggs"), inline = TRUE),
                 uiOutput("PartnersHSList"),
                 h3("Trade balance"),
                 plotOutput("PartnersTB"),
                 h3("Import"),
                 plotly::plotlyOutput("PrtnersImportPlot"),
                 h3("Export"),
                 plotly::plotlyOutput("PrtnersExportPlot")),
        tabPanel("Data table",
                 DT::dataTableOutput("PartnersHSTable")),
        tabPanel("Filters", verbatimTextOutput("filters"))
        
      ))
  )
))
