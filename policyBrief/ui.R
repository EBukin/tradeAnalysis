# A sufisticated application for trade data exploration.
# Developed by Eduard Bukin


library(shiny)
library(plyr)
library(dplyr)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)


# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Trade data explorer",
    tabPanel(
      "Partners", 
      sidebarLayout(
        sidebarPanel(
          selectInput("varType", "Variable:", choices = NULL),
          sliderInput("yearsRange", label = "Years range", min = 1995, max = 2016, value = c(2010, 2016), step = 1L),
          selectInput("reporters", "Reporter", choices = NULL, selectize = TRUE, multiple = FALSE),
          selectInput("partners", "Partner", choices = NULL, selectize = TRUE, multiple = FALSE)
          # numericInput("NPartners", label = "N top partners", value = 5, width = "100%"),
          # numericInput("NPeriods", label = "N periods for top partners", value = 3, width = "100%"),
          # checkboxInput("oneEUCB", "EU as one region", value = TRUE),
          # checkboxInput("oneFSRCB", "FSR as one region"),
          # checkboxInput("oneRUSCB", "Russia separately")#,
          # selectInput("reporters", "Reporter", setNames(tradeAnalysis::getFSR()$Partner.Code, tradeAnalysis::getFSR()$Partner), selectize = TRUE, multiple = FALSE,  selected =  c(804) )
          
          ),
        # Show actions in the tabs
        mainPanel(tabsetPanel(
          tabPanel(
            "Top partners",
            fluidPage(
            fluidRow(
              column(3, selectInput("comGroup", "Group:", choices = NULL)),
              column(9, selectInput("comCode", "Commodity:", choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"))
              ),
            
            # radioButtons(
            #   "PartnersHSTypes",
            #   "Types of HS codes to present",
            #   choices = c(
            #     "WTO AgriFood" = "Aggs",
            #     "Main components of WTO AgriFood" = "MainAgFood",
            #     "Relevant to WTO AgriFood 2-6 digits codes" = "RelevantAgFood",
            #     "All available" = "All"
            #   ),
            #   selected = c("Aggs"),
            #   inline = TRUE
            # ),
            uiOutput("PartnersHSList"),
            h3("Trade balance"),
            plotOutput("PartnersTB"),
            h3("Import"),
            # plotly::plotlyOutput("PrtnersImportPlot"),
            h3("Export")#,
            # plotly::plotlyOutput("PrtnersExportPlot")
          )),
          tabPanel("Data table",
                   DT::dataTableOutput("PartnersHSTable")),
          tabPanel("Filters", verbatimTextOutput("filters"))
          
        ))),
      tabPanel("Commodities"),
      tabPanel("Data availability")
  ))
  
  # fluidPage(
  # # Application title
  # titlePanel("COMTRADE data availability"),
  # 
  # # Sidebar with a slider input for number of bins

  # )
)#)
