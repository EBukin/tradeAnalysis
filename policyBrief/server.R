#
# Developed by Eduard Bukin

library(shiny)
library(data.table)
library(plyr)
library(tidyverse)
library(jsonlite)
library(stringr)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)
library(plotly)

# Loading data
# load("tradeDataExplorer/ctAnWTOFSR.Rdata")
# load("tradeDataExplorer/wtoAgFood.Rdata")
# load("policyBrief/appDF.RData")

load("appDF.RData")
# load("wtoAgFood.Rdata")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output, clientData, session) {
    
    # Loading HS codes groupping table
    hsGroups <-
      read_csv(system.file("extdata", "HS_agg_groups.csv", package = "tradeAnalysis"),
               col_types = cols(
                 Commodity.Code = col_character(),
                 Commodity = col_character(),
                 Group = col_character()
               )) %>% 
      select(Commodity.Code, Group)
    
    # Loading data
    df <- 
      df %>% 
      left_join(hsGroups, "Commodity.Code")
    
    # List of available HS codes
    commodityGroups <-
      df %>% 
      select(Commodity.Code, Group) %>% 
      distinct() %>% 
      arrange(Group, Commodity.Code) %>% 
      mutate(Group = ifelse(is.na(Group), "Other", Group))
    
    # Updating a potential choice of commodities
    updateSelectInput(
      session,
      inputId = "comGroup",
      choices = unique(commodityGroups$Group),
      selected = unique(commodityGroups$Group)[1]
    )
    
    # Global data characteristics:
    dataGlobal <-
      reactive({
        data <- df
        vars <- 
          data %>% 
          select_(.dots = names(.)[names(.) %in% c("Variable", "Type")]) %>% 
          distinct() %>% 
          unite(Variable, Variable, Type, sep = ", ") %>% 
          .$Variable
        repsList <- 
          data %>%
          select(Region, Reporter.Code) %>% 
          distinct() %>% 
          dlply(.,.(Region), function(x) {x$Reporter.Code})
        list(reporters = repsList,
             varType = vars)
      })

    # Filtering data depends on the commodity group choice
    dataCommodity <- reactive({
      if(is.null(input$comGroup)) return()
      if(is.null(input$varType)) return()
      if(is.null(input$reporters)) return()
      if(is.null(input$yearsRange)) return()
      
      data <-
        df %>% 
        filter(Group %in% c(input$comGroup))#,
               # Variable %in% tibble(var = input$varType) %>% separate(var, c("Variable", "Type"), ", ") %>% .$Variable,
               # Type %in% tibble(var = input$varType) %>% separate(var, c("Variable", "Type"), ", ") %>% .$Type,
               # Year %in% c(input$yearsRange[1]:input$yearsRange[2]))
      
      comList <-
        data %>%
        select(Commodity.Code, Commodity) %>%
        distinct() %>%
        arrange(Commodity) %>% 
        mutate(Commodity = str_trunc(Commodity, 30))
      
      comList <-
        setNames(comList$Commodity.Code, comList$Commodity)
      
      list(data = data,
           comCodesList = comList)
    })
    
    observe({
      # Select variable type input (Value - Quantity)
      updateSelectInput(
        session,
        inputId = "varType",
        choices = dataGlobal()$varType,
        selected = dataGlobal()$varType[1])
    })
    observe({
      # Updating a potential choice of reporters
      updateSelectInput(
        session,
        inputId = "reporters",
        choices = dataGlobal()$reporters
      )
    })
    observe({
      # Updating the list of commodities
      updateSelectInput(
        session,
        inputId = "comCode",
        choices = dataCommodity()$comCodesList,
        selected = dataCommodity()$comCodesList
      )
    })
    

    
      # Generating UI part with the commodities related to a partner code
  #   output$PartnersHSList <-
  #     renderUI({
  #       HSTypes <- input$PartnersHSTypes
  #     
  #     # Exctracting commodties
  #     HSList <-
  #       ctdata %>%
  #       select(Commodity.Code) %>%
  #       distinct() %>%
  #       arrange(desc(str_length(Commodity.Code))) %>%
  #       join_lables()
  #     
  #     aggs <-
  #       tibble(
  #         Commodity.Code = c(
  #           "WTO_AgriFood",
  #           "1_24_Total_excl_fish",
  #           "3_Fish",
  #           "29_53_AgriFoodGoods",
  #           "29_38_Chemicals",
  #           "41_43_Skins",
  #           "50_Silk",
  #           "51_Wool",
  #           "52_Cotton",
  #           "53_Fibers"
  #         )
  #       )
  #     # Ordering and filtering commodities
  #     HSList <-
  #       HSList %>%
  #       filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
  #       right_join(aggs,
  #                  "Commodity.Code") %>%
  #       bind_rows(HSList %>%
  #                   filter(!is.na(suppressWarnings(
  #                     as.integer(Commodity.Code)
  #                   ))) %>%
  #                   arrange(Commodity.Code)) %>% 
  #       mutate(Commodity = str_trunc(Commodity, width = 60))
  #     # Filtering commodities
  #     if (HSTypes == "Aggs") {
  #       HSList <-
  #         HSList %>%
  #         filter(Commodity.Code %in% aggs$Commodity.Code[1:4])
  #     } else if (HSTypes == "MainAgFood") {
  #       HSList <-
  #         HSList %>%
  #         filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
  #         bind_rows(HSList %>%
  #                     filter(Commodity.Code %in% wtoAgFood$Commodity.Code))
  #     } else if (HSTypes == "RelevantAgFood") {
  #       HSList <-
  #         HSList %>%
  #         filter(is.na(suppressWarnings(as.integer(Commodity.Code)))) %>%
  #         bind_rows(HSList %>%
  #                     filter(Commodity.Code %in% wtoAgFoodFull$Commodity.Code))
  #     } else {
  #       HSList <- HSList
  #     }
  #     # Preparing a list of items
  #     HSList <-
  #       setNames(HSList$Commodity.Code, HSList$Commodity)
  #     # Generating the UI
  #     selectInput(
  #       "PartnersHSList",
  #       "Select HS code(s) to compare:",
  #       HSList,
  #       selectize = TRUE,
  #       multiple = TRUE,
  #       selected =  HSList[1],
  #       width = '100%'
  #     )
  #   })
  # 
  # # Filtering a country selected
  # dfCountryYear <- reactive({
  #   type <- input$dataType
  #   area <- as.integer(input$reporters)
  #   year <- as.integer(input$yearsRange[1]):as.integer(input$yearsRange[2])
  #   if(is.null(input$PartnersHSList)) return()
  #   df <- ctdata %>% 
  #     filter(Commodity.Code %in% input$PartnersHSList)
  #   if (!is.null(area)) {
  #     df <- 
  #       df %>%
  #       filter(Reporter.Code %in% area)
  #   } 
  #   if (all(!is.null(year))) {
  #     df <- 
  #       df %>%
  #       filter(Year %in% year)
  #   }     
  #   if (all(!is.null(type))) {
  #     df <- 
  #       df %>%
  #       filter(Variable %in% type)
  #   } 
  #   df %>% 
  #     dplyr::group_by_(.dots = names(df)[!(names(df) %in% c("Value", "Commodity.Code", "Commodity"))] ) %>% 
  #     dplyr::summarise(Value = sum(Value)) %>% 
  #     dplyr::ungroup() 
  # })
  
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
