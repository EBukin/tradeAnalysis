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
          select(Region, Reporter, Reporter.Code) %>% 
          distinct() %>% 
          mutate(Reporter = ifelse(is.na(Reporter), Reporter.Code, Reporter)) %>% 
          dlply(., .(Region), function(x) {setNames(x$Reporter.Code, x$Reporter)})
        list(reporters = repsList,
             varType = vars)
      })
    
    # Returnb partners list
    dataPartners <-
      reactive({
 
        year <- as.integer(input$yearsRange[1]):as.integer(input$yearsRange[2])
        area <- input$reporters
        
        data <- df
        
        if (!is.null(area)) {
          data <-
            data %>%
            filter(Reporter.Code %in% area)
        }
        

        if (all(!is.null(year))) {
          data <-
            data %>%
            filter(Year %in% year)
        }
        vars <- 
          data %>% 
          select(Partner.Code, Partner) 
        list(data = data, partners = setNames(vars$Partner.Code, vars$Partner))
      })
    
    # Filtering data depends on the commodity group choice
    dataCommodity <- reactive({
      partnersArea <- input$partners
      data <-
        dataPartners()$data 
      comList <-
        data %>%
        select(Commodity.Code, Commodity) %>%
        distinct() %>%
        arrange(Commodity) %>%
        mutate(Commodity = str_trunc(Commodity, 30))
      comList <-
        setNames(comList$Commodity.Code, comList$Commodity)
      list(comCodesList = comList)
    })
    

    observe({
      # Select variable type input (Value - Quantity)
      updateSelectInput(
        session,
        inputId = "varType",
        choices = dataGlobal()$varType,
        selected = dataGlobal()$varType[1])
      # Updating a potential choice of reporters
      updateSelectInput(
        session,
        inputId = "reporters",
        choices = dataGlobal()$reporters
      )
      updateSelectInput(
        session,
        inputId = "partners",
        choices = dataPartners()$partners
      )
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
  # Filtering a country selected
  # dfCountryYear <- reactive({
  #   area <- input$reporters
  #   year <- as.integer(input$yearsRange[1]):as.integer(input$yearsRange[2])
  #   df <- dataCommodity()$data
  #   
  #   if (!is.null(area)) {
  #     df <-
  #       df %>%
  #       filter(Reporter.Code %in% area)
  #   }
  #   
  #   if (all(!is.null(year))) {
  #     df <-
  #       df %>%
  #       filter(Year %in% year)
  #   }
  #   
  #   df #%>%
  #     # dplyr::group_by_(.dots = names(df)[!(names(df) %in% c("Value", "Commodity.Code", "Commodity"))] ) %>%
  #     # dplyr::summarise(Value = sum(Value)) %>%
  #     # dplyr::ungroup()
  #   
  # })
  
  generatePlots <- 
    reactive({
      # if(nrow(dfCountryYear()) == 0) return()
      if(is.null(input$comGroup)) return()

      data <-
        dataPartners()$data %>%
        filter(Group %in% c(input$comGroup))
 
      # TradeBalance <- 
      #   data %>%
      #   plot_tb(
      #     stackVar = "Commodity.Code",
      #     brewScale = T,
      #     brewPalName = "Set2",
      #     plotTradeBalance = TRUE, 
      #     returnData = TRUE
      #   ) 
      # import <- 
      #   data %>%
      #   filter(Trade.Flow.Code == 1) %>%
      #   plot_tb(
      #     stackVar = "Commodity.Code",
      #     brewScale = T,
      #     brewPalName = "Set2",
      #     plotTradeBalance = FALSE, 
      #     returnData = FALSE
      #   ) 
      # export <- 
      #   data %>%
      #   filter(Trade.Flow.Code == 2) %>%
      #   plot_tb(
      #     stackVar = "Commodity.Code",
      #     brewScale = T,
      #     brewPalName = "Set2",
      #     plotTradeBalance = FALSE, 
      #     returnData = FALSE
      #   ) 
      # 
      # list(df = TradeBalance$data, TB = TradeBalance$plot, Imp = import, Exp = export)
    })
  # 
  # output$PartnersHSTable<-
  #   DT::renderDataTable({
  #     generatePlots()$df %>% 
  #       select(-order) %>% 
  #       filter(Trade.Flow != "Trade balance") %>% 
  #       arrange(Trade.Flow, Partner) %>% 
  #       spread(Period, Value)
  #   })
  # 
  # 
  # output$PrtnersImportPlot <-
  #   renderPlotly({
  #     generatePlots()$Imp %>% 
  #       ggplotly() %>% 
  #       print()
  #   })
  # 
  # output$PrtnersExportPlot <-
  #   renderPlotly({
  #     generatePlots()$Exp %>% 
  #       ggplotly() %>% 
  #       print()
  #   })
  # 
  # 
  # output$PartnersTB <- renderPlot({
  #   generatePlots()$TB %>% 
  #     print
  # })
  

})
