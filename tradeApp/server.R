#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(data.table)
library(plyr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)
library(shiny)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)
# install.packages("../tradeAnalysis_0.0.4Q.tar.gz")
# load("tradeApp/availability.rdata")
load("availability.rdata")

# ctAval <-
#   jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
#   tbl_df()
# 
# ctAval <-
#   ctAval %>%
#   mutate(Year = as.integer(str_sub(ps, 1, 4)),
#          Month = as.integer(ifelse(str_length(ps) > 4, str_sub(ps, 5, 6), NA)),
#          ps = str_c(str_sub(ps, 1, 4), "-", str_sub(ps, 5, 6)))
# 
# ctAvalBP <- ctAval
# if(!file.exists("availability.rdata")) {
#   ctAval <-
#     jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
#     tbl_df() %>%
#     mutate(
#       Year = as.integer(str_sub(ps, 1, 4)),
#       Month = as.integer(ifelse(str_length(ps) > 4, str_sub(ps, 5, 6), NA)),
#       ps = str_c(str_sub(ps, 1, 4), "-", str_sub(ps, 5, 6))
#     )
#   save(ctAval, file = "availability.rdata")
# } else {
#   load("availability.rdata")
# }

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  # Selecting full list of countries
  countriesFilter <- ctAval %>% select(r) %>% distinct() %>% join_lables() %>% filter(!is.na(r))
  countriesFilter <- setNames(countriesFilter$r, countriesFilter$Reporter)
  
  # Rendering uiCountries selector 
  output$uiCountries <- renderUI({
    # if (length(countriesFilter) == 0) return()
    selectInput("countries", "Countries:", countriesFilter, selectize = TRUE, multiple = TRUE, selected = "ALL")
  })
  
  dataCountries <- reactive({
    ctAval %>% 
      filter(r %in% input$countries) %>% 
      return()
  })
  
  # Rendering uiPeriods 
  output$uiPeriodicity <- renderUI({
    frequencies <- unique(dataCountries()$freq)
    frequencies <- setNames(frequencies, frequencies)
    selectInput("periodicity", "Periodicity:", frequencies)
  })
  
  # Rendering uiMerchant 
  output$uiMerchant <- renderUI({
    type <- unique(dataCountries()$type)
    type <- setNames(type, type)
    selectInput("merchandize", "Merchandize type:", type, selectize = TRUE)
  })
  
  # Rendering uiMerchant 
  output$uiCoding <- renderUI({
    coding <- unique(dataCountries()$px)
    coding <- setNames(coding, coding)
    selectInput("system", "Coding system:", coding, selected = "HS", selectize = TRUE)
  })
  
  # Rendering uiYear 
  output$uiYear <- renderUI({
    Years <- unique(dataCountries()$Year)
    sliderInput("yearsRange", label = h3("Years range"), min = min(Years), max = max(Years),
                value = c(max(Years) - 5, max(Years)), step = 1L)
  })
  
  data <- 
    reactive({
      period <- as.character(input$periodicity)
      merchant <- as.character(input$merchandize)
      codingSystem <- as.character(input$system)
      years <- seq(input$yearsRange[1], input$yearsRange[2], 1)
      
      # Data filtering
      data <- 
       ctAval %>% 
       filter(freq == period,
              type == merchant,
              px == codingSystem,
              Year %in% years)
      # 
      # if(period != "ANNUAL") {
      #   months <- seq(input$monthRange[1], input$monthRange[2], 1)
      #   data <- data %>% filter(Month %in% months)
      # } 
      # 
      # if("FSR" %in% countries) {
      #   countries <- c(countries[!countries %in% "FSR"], 
      #                  51, 31, 112, 268, 398, 417, 498, 643, 762, 795, 804, 860)
      # }
      # 
      # if(any(countries != "ALL")) {
      #   data <- data %>% filter(r %in% as.character(countries))
      # } else {
      #   data <- data
      # }
      # 
      # # Reshaping results 
      # data <-
      #   data %>% 
      #   select(r, ps) %>% 
      #   mutate(Value = "X") %>% 
      #   spread(ps, Value)
      # 
      # # Returning the results
      # list(data = data, 
      #      filters = list(period = period,
      #                     merchant = merchant,
      #                     codingSystem = codingSystem,
      #                     years = years,
      #                     months = months,
      #                     countries = countries))
      })
  
  output$table <- renderTable({data()$data})
  output$filters <- renderPrint({data()$filters})
  
})


