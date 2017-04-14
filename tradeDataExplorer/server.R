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
# load("tradeDataExplorer/ctAnWTOFSR.Rdata")
# load("tradeDataExplorer/wtoAgFood.Rdata")

load("ctAnWTOFSR.Rdata")
load("wtoAgFood.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # # Loading all data
  # # df <- function() {ctdata}
  # dataFull <-
  #   reactive({
  #     input$periodicity 
  #     # browser()
  #     # load("ctAnWTOFSR.Rdata", envir = environment())
  #     # if(input$periodicity == "MONTHLY") {
  #     #   load("ctAnWTOFSR.Rdata", envir = environment())
  #     # } else {
  #     #   load("ctAnWTOFSR.Rdata", envir = environment())
  #     # }
  #     # ctdata
  #   })
  
  # Selecting a list of available countries
  countriesFilter <-
    ctdata %>% 
    select(Reporter.Code) %>% 
    distinct() %>% 
    join_lables() %>% 
    arrange(Reporter)
  
  countriesFilter <-
    setNames(countriesFilter$Reporter.Code, countriesFilter$Reporter)
  
  # Rendering uiCountries selector
  output$uiCountries <- renderUI({
    # if (length(countriesFilter) == 0) return()
    selectInput(
      "countries",
      "Select a country:",
      countriesFilter,
      selectize = TRUE,
      multiple = FALSE, 
      selected =  c(804)
    )
  })
  
  # Filtering a country selected
  dfCountry <- reactive({
    area <- as.integer(input$countries)
    if (!is.null(area)) {
      ctdata %>%
        filter(Reporter.Code %in% area)
    } else {
      ctdata
    }
  })
  
  # ctdata
  
  # Rendering uiYear
  output$uiYear <- renderUI({
    # browser()
    Years <- unique(dfCountry()$Year)
    if(length(Years) == 0) {Years <- c(2000:2020)}
    sliderInput(
      "yearsRange",
      label = "Years range",
      min = min(Years),
      max = max(Years),
      value = c(max(Years) - 5, max(Years)),
      step = 1L
    )
  })
  
  # Filtering years from data
  dfCountryYear <-
    reactive({
      year <-
        as.integer(input$yearsRange[1]):as.integer(input$yearsRange[2])
      if (all(!is.null(year))) {
        dfCountry() %>%
          filter(Year %in% year)
      } else {
        dfCountry()
      }
    })
  

  availCodes <- reactive({
      wtoAgFoodFull %>% 
        right_join(dfCountryYear() %>% 
                     select(Commodity.Code) %>% 
                     distinct(), 
                   by = "Commodity.Code") %>% 
        arrange(Commodity.Code) %>% 
        filter(str_count(Commodity.Code) %in% input$HSdigits) %>% 
      mutate(Commodity = str_trunc(Commodity, 60))
    
  })
  
  availFiltCodes <- reactive({
    if(is.null(input$HSdigits)) {
      availCodes()
    } else {
      availCodes() %>% 
        filter(str_count(Commodity.Code) %in% input$HSdigits) 
    }
    
  })
  
  
  output$table <- renderTable({
    availFiltCodes()
  })
  output$filters <- renderPrint({
    str(availFiltCodes())
   
  })
  
  
  # # Rendering uiPeriods
  # output$uiPeriodicity <- renderUI({
  #   frequencies <- unique(dfCountry()$freq)
  #   frequencies <- setNames(frequencies, frequencies)
  #   selectInput("periodicity", "Periodicity:", frequencies, selected = "ANNUAL")
  # })

  # # Rendering uiCoding
  # output$uiCoding <- renderUI({
  #   coding <- unique(dfCountry()$px)
  #   coding <- setNames(coding, coding)
  #   selectInput("system",
  #               "Coding system:",
  #               coding,
  #               selected = "HS",
  #               selectize = TRUE)
  # })
  

  # data <-
  #   reactive({
  #     if(all(!is.null(input$periodicity), 
  #            !is.null(input$merchandize),
  #            !is.null(input$system),
  #            !is.null(input$yearsRange[1]))) {
  #       data <-
  #         dataCountries() %>%
  #         filter(
  #           freq == as.character(input$periodicity),
  #           type == as.character(input$merchandize),
  #           px == as.character(input$system),
  #           Year >= input$yearsRange[1],
  #           Year <= input$yearsRange[2]
  #         ) %>%
  #         select(r, ps) %>%
  #         mutate(Value = "X")  %>%
  #         complete(r = input$countries, ps) %>% 
  #         join_lables() %>% 
  #         arrange(Reporter)
  #       
  #       data <-
  #         bind_rows(data, 
  #                   data %>% 
  #                     filter(!is.na(Value)) %>% 
  #                     group_by(ps) %>% 
  #                     summarise(r = NA, 
  #                               Reporter = "Total number",
  #                               Value = n()) %>% 
  #                     ungroup() %>% 
  #                     mutate(Value = as.character(Value)))  %>% 
  #         replace_na(list(Value = "")) %>% 
  #         spread(ps, Value)
  #       
  #       # Returning the results
  #       list(
  #         data = data,
  #         filters = list(
  #           countries = input$countries,
  #           period = as.character(input$periodicity),
  #           merchant = as.character(input$merchandize),
  #           codingSystem = as.character(input$system),
  #           years = input$yearsRange[1]:input$yearsRange[2]
  #         )
  #       )
  #     } else {
  #       list(
  #         data = NA,
  #         filters = NA)
  #     }
  #     
  #   })
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("ct-data-availability-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(x = data()$data, file)
  #   })
  

})
