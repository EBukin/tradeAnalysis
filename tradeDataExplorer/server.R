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
library(dplyr)
library(tidyverse)
library(jsonlite)
library(stringr)
library(shiny)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)
library(plotly)

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
  # Aggegating data and extracting all commodities
  # Selecting a list of available countries
  
  ctdata <- 
    ctdata %>% 
    agg_commodities(., onlyAggregates = FALSE)

  # Generating UI part with the commodities related to a partner code
  output$PartnersHSList <-
    renderUI({
      HSTypes <- input$PartnersHSTypes
      
      # Exctracting commodties
      HSList <-
        ctdata %>%
        select(Commodity.Code) %>%
        distinct() %>%
        arrange(desc(str_length(Commodity.Code))) %>%
        join_lables()
      
      aggs <-
        tibble(
          Commodity.Code = c(
            "WTO_AgriFood",
            "1_24_Total_excl_fish",
            "3_Fish",
            "29_53_AgriFoodGoods",
            "29_38_Chemicals",
            "41_43_Skins",
            "50_Silk",
            "51_Wool",
            "52_Cotton",
            "53_Fibers"
          )
        )
      # Ordering and filtering commodities
      HSList <-
        HSList %>%
        filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
        right_join(aggs,
                   "Commodity.Code") %>%
        bind_rows(HSList %>%
                    filter(!is.na(suppressWarnings(
                      as.integer(Commodity.Code)
                    ))) %>%
                    arrange(Commodity.Code)) %>% 
        mutate(Commodity = str_trunc(Commodity, width = 60))
      # Filtering commodities
      if (HSTypes == "Aggs") {
        HSList <-
          HSList %>%
          filter(Commodity.Code %in% aggs$Commodity.Code[1:4])
      } else if (HSTypes == "MainAgFood") {
        HSList <-
          HSList %>%
          filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
          bind_rows(HSList %>%
                      filter(Commodity.Code %in% wtoAgFood$Commodity.Code))
      } else if (HSTypes == "RelevantAgFood") {
        HSList <-
          HSList %>%
          filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
          bind_rows(HSList %>%
                      filter(Commodity.Code %in% wtoAgFoodFull$Commodity.Code))
      } else {
        HSList <- HSList
      }
      # Preparing a list of items
      HSList <-
        setNames(HSList$Commodity.Code, HSList$Commodity)
      # Generating the UI
      selectInput(
        "PartnersHSList",
        "Select HS code(s) to compare:",
        HSList,
        selectize = TRUE,
        multiple = TRUE,
        selected =  HSList[1],
        width = '100%'
      )
    })
  
  # Filtering a country selected
  dfCountryYear <- reactive({
    type <- input$dataType
    area <- as.integer(input$reporters)
    year <- as.integer(input$yearsRange[1]):as.integer(input$yearsRange[2])
    if(is.null(input$PartnersHSList)) return()
    df <- ctdata %>% 
      filter(Commodity.Code %in% input$PartnersHSList)
    if (!is.null(area)) {
      df <- 
        df %>%
        filter(Reporter.Code %in% area)
    } 
    if (all(!is.null(year))) {
      df <- 
        df %>%
        filter(Year %in% year)
    }     
    if (all(!is.null(type))) {
      df <- 
        df %>%
        filter(Variable %in% type)
    } 
    df %>% 
      dplyr::group_by_(.dots = names(df)[!(names(df) %in% c("Value", "Commodity.Code", "Commodity"))] ) %>% 
      dplyr::summarise(Value = sum(Value)) %>% 
      dplyr::ungroup() 
  })
  
  generatePlots <- 
    reactive({
      if(is.null(dfCountryYear())) return()
      # browser()
      df <- 
        dfCountryYear() %>%
        filter(Partner.Code != 0) %>%
        rank_agg_top_partners(
          top_n = input$NPartners,
          agg = TRUE,
          oneEU = input$oneEUCB,
          oneFSR = input$oneFSRCB,
          oneRUS = input$oneRUSCB, 
          topPeriod = input$NPeriods 
        )  
      
      TradeBalance <- 
        df %>%
        plot_tb(
          stackVar = "Partner",
          brewScale = T,
          brewPalName = "Set2",
          plotTradeBalance = TRUE, 
          returnData = TRUE
        ) 
      import <- 
        df %>%
        filter(Trade.Flow.Code == 1) %>%
        plot_tb(
          stackVar = "Partner",
          brewScale = T,
          brewPalName = "Set2",
          plotTradeBalance = FALSE, 
          returnData = FALSE
        ) 
      export <- 
        df %>%
        filter(Trade.Flow.Code == 2) %>%
        plot_tb(
          stackVar = "Partner",
          brewScale = T,
          brewPalName = "Set2",
          plotTradeBalance = FALSE, 
          returnData = FALSE
        ) 

      list(df = TradeBalance$data, TB = TradeBalance$plot, Imp = import, Exp = export)
    })
  
  output$PartnersHSTable<-
    DT::renderDataTable({
      generatePlots()$df %>% 
        select(-order) %>% 
        filter(Trade.Flow != "Trade balance") %>% 
        arrange(Trade.Flow, Partner) %>% 
        spread(Period, Value)
    })
  
  
  output$PrtnersImportPlot <-
    renderPlotly({
      generatePlots()$Imp %>% 
        ggplotly() %>% 
        print()
    })
  
  output$PrtnersExportPlot <-
    renderPlotly({
      generatePlots()$Exp %>% 
        ggplotly() %>% 
        print()
    })
  
  
  output$PartnersTB <- renderPlot({
    generatePlots()$TB %>% 
      print
  })
  

})
