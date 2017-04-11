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
if(!file.exists("availability.rdata")) {
  ctAval <-
    jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
    tbl_df() %>%
    mutate(
      Year = as.integer(str_sub(ps, 1, 4)),
      Month = as.integer(ifelse(str_length(ps) > 4, str_sub(ps, 5, 6), NA)),
      ps = str_c(str_sub(ps, 1, 4), "-", str_sub(ps, 5, 6))
    )
  save(ctAval, file = "availability.rdata")
} else {
  load("availability.rdata")
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  observeEvent(input$updtCT, { 
    ctAval <-
      jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
      tbl_df() %>%
      mutate(Year = as.integer(str_sub(ps, 1, 4)),
             Month = as.integer(ifelse(str_length(ps) > 4, str_sub(ps, 5, 6), NA)),
             ps = str_c(str_sub(ps, 1, 4), "-", str_sub(ps, 5, 6)))
    save(ctAval, file = "availability.rdata")
    load("availability.rdata")
    })
  
  data <- 
    reactive({
      period <- as.character(input$periodicity)
      merchant <- as.character(input$merchandize)
      codingSystem <- as.character(input$system)
      years <- seq(input$yearsRange[1], input$yearsRange[2], 1)
      countries <- as.character(input$countries)
      months <- NA
      if(any(is.null(countries))) {countries <- "ALL"}
      
      # Data filtering
      data <- 
        ctAval %>% 
        filter(freq == period,
               type == merchant,
               px == codingSystem,
               Year %in% years)
      
      if(period != "ANNUAL") {
        months <- seq(input$monthRange[1], input$monthRange[2], 1)
        data <- data %>% filter(Month %in% months)
      } 
      
      if("FSR" %in% countries) {
        countries <- c(countries[!countries %in% "FSR"], 
                       51, 31, 112, 268, 398, 417, 498, 643, 762, 795, 804, 860)
      }
      
      if(any(countries != "ALL")) {
        data <- data %>% filter(r %in% as.character(countries))
      } else {
        data <- data
      }
      
      # Reshaping results 
      data <-
        data %>% 
        select(r, ps) %>% 
        mutate(Value = "X") %>% 
        spread(ps, Value)
      
      # Returning the results
      list(data = data, 
           filters = list(period = period,
                          merchant = merchant,
                          codingSystem = codingSystem,
                          years = years,
                          months = months,
                          countries = countries))
      })
  
  output$table <- renderTable({data()$data})
  output$filters <- renderPrint({data()$filters})
  
})
