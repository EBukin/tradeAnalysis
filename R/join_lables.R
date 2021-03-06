#' Add lables of codes to the trade data
#'
#'  @param mappingTbls Path to the Rdata file with mapping tables.
#'  @param keepCodes Logical variable for keeping or removing the codes.
join_lables <-
  function(data,
           mappingTbls = "data/ctClass.Rdata",
           commAggMT = "data/HS_agg_names.csv" ,
           keepCodes = TRUE) {
    require(plyr)
    require(dplyr)
    load(mappingTbls)
    class <-
      bind_rows(class, read.csv(commAggMT, stringsAsFactors = FALSE))
    oldNames <- c("r",
                  "Reporter.Code",
                  "Partner.Code",
                  "Trade.Flow.Code",
                  "Qty.Unit.Code")
    newNames <-
      c("Reporter", "Partner", "Trade.Flow", "Unit", "Commodity")
    
    data <-
      data %>%
      select_(.dots = names(.)[!names(.) %in% newNames])
    
    # Reporters names
    if ("Reporter.Code" %in% names(data)) {
      data <- data %>%
        mutate(Reporter.Code = as.integer(Reporter.Code)) %>%
        left_join(rep, by = "Reporter.Code")
    }
    
    # Reporters names
    if ("r" %in% names(data)) {
      data <- data %>%
        mutate(r = as.integer(r)) %>%
        left_join(rep, by = c("r" = "Reporter.Code"))
    }
    
    # Partners names
    if ("Partner.Code" %in% names(data)) {
      data <- data %>%
        mutate(Partner.Code = as.integer(Partner.Code)) %>%
        left_join(part, by = "Partner.Code") %>%
        mutate(Partner = ifelse(is.na(Partner), "ROW", Partner))
    }
    
    # Trade flows names
    if ("Trade.Flow.Code" %in% names(data)) {
      data <- data %>%
        mutate(Trade.Flow.Code = as.integer(Trade.Flow.Code)) %>%
        left_join(reg, by = "Trade.Flow.Code")
    }
    
    if ("Qty.Unit.Code" %in% names(data)) {
      data <- data %>%
        mutate(Qty.Unit.Code = as.integer(Qty.Unit.Code)) %>%
        left_join(units %>% select(-Unit.Description),
                  by = "Qty.Unit.Code")
    }
    
    if ("Commodity.Code" %in% names(data)) {
      data <- data %>%
        mutate(Commodity.Code = as.character(Commodity.Code)) %>%
        left_join(class, by = "Commodity.Code")
    }
    
    if (!keepCodes) {
      data <-
        data %>%
        select_(.dots = names(.)[!names(.) %in% oldNames])
    }
    
    return(data)
  }
