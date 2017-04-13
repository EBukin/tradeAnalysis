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
library(jsonlite)
library(stringr)
library(shiny)
devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)

# Loading data
load("availability.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Update data availability
  # observeEvent(input$updtCT, {
  #   ctAval <-
  #     jsonlite::fromJSON("http://comtrade.un.org/api/refs/da/bulk?parameters") %>%
  #     tbl_df() %>%
  #     mutate(
  #       Year = as.integer(str_sub(ps, 1, 4)),
  #       Month = as.integer(ifelse(str_length(ps) > 4, str_sub(ps, 5, 6), NA)),
  #       ps = str_c(str_sub(ps, 1, 4), "-", str_sub(ps, 5, 6))
  #     )
  #   save(ctAval, file = "availability.rdata")
  #   load("availability.rdata")
  #   
  #   # Selecting full list of countries
  #   countriesFilter <-
  #     ctAval %>% 
  #     select(r) %>% 
  #     distinct() %>% 
  #     join_lables() %>% 
  #     filter(!is.na(r), !is.na(Reporter)) %>% 
  #     arrange(Reporter)
  #   countriesFilter <-
  #     setNames(countriesFilter$r, countriesFilter$Reporter)
  #   
  # })
  
  # Selecting full list of countries
  countriesFilter <-
    ctAval %>% 
    select(r) %>% 
    distinct() %>% 
    join_lables() %>% 
    filter(!is.na(r), !is.na(Reporter)) %>% 
    arrange(Reporter)
  countriesFilter <-
    setNames(countriesFilter$r, countriesFilter$Reporter)
  
  # Rendering uiCountries selector
  output$uiCountries <- renderUI({
    # if (length(countriesFilter) == 0) return()
    selectInput(
      "countries",
      "Countries:",
      countriesFilter,
      selectize = TRUE,
      multiple = TRUE, 
      selected =  c("643", "804", "860", "762", "795", 
                    "31", "268", "498", "51", "112", "398")
    )
  })
  
  dataCountries <- reactive({
    if (!is.null(input$countries)) {
      ctAval %>%
        filter(r %in% input$countries)
    } else {
      ctAval
    }
  })
  
  # Rendering uiPeriods
  output$uiPeriodicity <- renderUI({
    frequencies <- unique(dataCountries()$freq)
    frequencies <- setNames(frequencies, frequencies)
    selectInput("periodicity", "Periodicity:", frequencies, selected = "ANNUAL")
  })
  
  # Rendering uiMerchant
  output$uiMerchant <- renderUI({
    type <- unique(dataCountries()$type)
    type <- setNames(type, type)
    selectInput("merchandize", "Merchandize type:", type, selected = type[1], selectize = TRUE)
  })
  
  # Rendering uiCoding
  output$uiCoding <- renderUI({
    coding <- unique(dataCountries()$px)
    coding <- setNames(coding, coding)
    selectInput("system",
                "Coding system:",
                coding,
                selected = "HS",
                selectize = TRUE)
  })
  
  # Rendering uiYear
  output$uiYear <- renderUI({
    Years <- unique(dataCountries()$Year)
    sliderInput(
      "yearsRange",
      label = h3("Years range"),
      min = min(Years),
      max = max(Years),
      value = c(max(Years) - 5, max(Years)),
      step = 1L
    )
  })
  
  data <-
    reactive({
      if(all(!is.null(input$periodicity), 
             !is.null(input$merchandize),
             !is.null(input$system),
             !is.null(input$yearsRange[1]))) {
        data <-
          dataCountries() %>%
          filter(
            freq == as.character(input$periodicity),
            type == as.character(input$merchandize),
            px == as.character(input$system),
            Year >= input$yearsRange[1],
            Year <= input$yearsRange[2]
          ) %>%
          select(r, ps) %>%
          mutate(Value = "X")  %>%
          complete(r = input$countries, ps) %>% 
          join_lables() %>% 
          arrange(Reporter)
        
        data <-
          bind_rows(data, 
                    data %>% 
                      filter(!is.na(Value)) %>% 
                      group_by(ps) %>% 
                      summarise(r = NA, 
                                Reporter = "Total number",
                                Value = n()) %>% 
                      ungroup() %>% 
                      mutate(Value = as.character(Value)))  %>% 
          replace_na(list(Value = "")) %>% 
          spread(ps, Value)
        
        # Returning the results
        list(
          data = data,
          filters = list(
            countries = input$countries,
            period = as.character(input$periodicity),
            merchant = as.character(input$merchandize),
            codingSystem = as.character(input$system),
            years = input$yearsRange[1]:input$yearsRange[2]
          )
        )
      } else {
        list(
          data = NA,
          filters = NA)
      }
      
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ct-data-availability-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(x = data()$data, file)
    })
  
  output$table <- renderTable({
    data()$data
  })
  output$filters <- renderPrint({
    data()$filters
  })
  
})
