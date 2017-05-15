# Working file withthe data analisys forthe policy brief.


# SETUP

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
# devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)

# Loading data

load("policyBrief/wtoAnAllMain.Rdata")
load("policyBrief/wtoAnAll.Rdata")

# Cleaning data by removing those cases, where Reporter == Partner
wtoAnAllMain <- 
  wtoAnAllMain #%>% 
  # filter(Reporter.Code != Partner.Code)

# Funciton for converting to real prices -------------------------------
realUSD <-
  function(data, deflator = "policyBrief/us_deflator.csv") {
    mt <-
      read_csv(deflator, col_types = cols(Year = col_integer(),
                                          Value = col_double())) %>% 
      mutate(Value = Value / 100)
    names(mt)[2] <- "deflator"
    data %>% 
      left_join(mt, by = "Year") %>% 
      mutate(Value = Value / deflator,
             Type = "Constant USD (2010)") %>% 
      select(-deflator)
  }


period_mean_value <- 
  function(data, nYears = 3) {
    years <- min(data$Year):max(data$Year)
    yearGroups <- 
      ldply(split(years, ceiling(seq_along(years)/nYears)), 
            function(x) {
              tibble(Year = x, yearGroup = str_c(c(min(x), max(x)), collapse = "-"))
            }, 
            .id = NULL) %>% 
      tbl_df()
    
    data  %>% 
      mutate(Year = as.integer(Year),
             Period = as.integer(Period)) %>% 
      left_join(yearGroups, "Year") %>% 
      mutate(Year = yearGroup,
             Period = yearGroup) %>% 
      select(-yearGroup) %>% 
      group_by_(.dots = names(.)[!names(.) %in% c("Value", "Classification")]) %>% 
      summarise(Value = mean(Value, na.rm = TRUE)) %>% 
      ungroup()
  }

# Regions MT 
joinRegion <- 
  function(data, 
           mt = system.file("extdata", "ct_regions.csv", package = "tradeAnalysis"),
           RegionsType = "Income"){
    regionsMT <-
      read.csv(mt, stringsAsFactors = FALSE) %>% 
      tbl_df() %>% 
      select(Code, contains(RegionsType, ignore.case = TRUE), -contains("source"))
    names(regionsMT)[2] <- "Region"
    data %>% 
      left_join(regionsMT, by = c("Reporter.Code" = "Code")) 
  }

hsGroups <-
  read_csv(system.file("extdata", "HS_agg_groups.csv", package = "tradeAnalysis"),
           col_types = cols(
             Commodity.Code = col_character(),
             Commodity = col_character(),
             Group = col_character()
           )) %>% 
  select(Commodity.Code, Group)



# Creating regions --------------------------------------------------------

# Developing Mapping tabeles from Countries to regions
# Regional aggregates are created based on WESP
# https://www.un.org/development/desa/dpad/publication/world-economic-situation-and-prospects-2017/

# Prosessing data ---------------------------------------------------------

# Aggregating WTO Commodities
wtoAnAllAgg <- 
  wtoAnAllMain %>% 
  agg_commodities(., onlyAggregates = T) %>% 
  bind_rows(wtoAnAll %>% 
              filter(str_length(Commodity.Code) == 4) %>% 
              agg_commodities(., onlyAggregates = T),
            filter(wtoAnAllMain, Commodity.Code == "TOTAL")) %>% 
  join_lables() 


# Aggregating WTO Commodities
# wtoAnAllAgg2 <- 
  # wtoAnAll %>% 
  # filter(str_length(Commodity.Code) == 4) %>% 
  # agg_commodities(., onlyAggregates = T)  

# Filtering only one commodity
AgFood <-
  wtoAnAllAgg %>% 
  filter(Commodity.Code %in% c("WTO_AgriFood", "TOTAL"))%>%
  select(-Commodity)

# 4,587,579 × 12

# Returning by income trade data -----------
# AgFood %>% 
#   agg_regions(., RegionsType = "Income") %>% 
#   select(-Commodity) %>% 
#   unite(Commodity.Code, Partner.Code ,Commodity.Code) %>% 
#   spread(Commodity.Code, Value) %>% 
#   mutate(Type = "Current USD")  %>% 
#   write.csv("policyBrief/tradeByIncome.csv", row.names = FALSE)

# Returning by income trade data -----------
df <- 
  bind_rows(
    agg_regions(AgFood, RegionsType = "NetFood"),
    agg_regions(AgFood, RegionsType = "Income"),
    agg_regions(AgFood, RegionsType = "Development"),
    agg_regions(AgFood, RegionsType = "LDC"),
    agg_regions(AgFood, RegionsType = "World"),
    agg_regions(AgFood, RegionsType = "WTO")) %>% 
  mutate(Type = "Current USD") %>% 
  bind_rows(realUSD(.)) %>% 
  mutate(Year = as.character(Year),
         Period = as.character(Period)) %>% 
  bind_rows(period_mean_value(.)) %>% 
  spread(Commodity.Code, Value) %>%
  mutate(Share_WTO_in_TOTAL = WTO_AgriFood / TOTAL) %>%
  gather(Commodity.Code, Value, TOTAL, WTO_AgriFood, Share_WTO_in_TOTAL)

save(df, file = "policyBrief/appDF.RData")

# Writing data for the EXCEL app -----------------
df %>% write.csv(str_c("policyBrief/tradeByRegions", Sys.Date(), ".csv"), row.names = FALSE, na = "0")

# Data for shiny app app 

# Filtering only one commodity
AgFood <-
  wtoAnAllAgg %>% 
  # filter(Commodity.Code %in% c("WTO_AgriFood", "TOTAL"))%>%
  select(-Commodity)


df <- 
  bind_rows(
    agg_regions(AgFood, RegionsType = "NetFood"),
    agg_regions(AgFood, RegionsType = "Income"),
    agg_regions(AgFood, RegionsType = "Development"),
    agg_regions(AgFood, RegionsType = "LDC"),
    agg_regions(AgFood, RegionsType = "World"),
    agg_regions(AgFood, RegionsType = "WTO"),
    mutate(filter(AgFood, Partner.Code ==0) %>% 
             joinRegion(),
           # Region = "All Countries",
           Reporter.Code = as.character(Reporter.Code),
           Partner.Code = as.character(Partner.Code)) 
    ) %>% 
  mutate(Type = "Current USD") %>% 
  bind_rows(realUSD(.)) %>%
  mutate(Year = as.character(Year),
         Period = as.character(Period)) %>% 
  bind_rows(period_mean_value(.)) %>% 
  right_join(hsGroups, "Commodity.Code")

df <-
  df %>% 
    # join_lables()
    # filter(!is.na(Reporter))
  select(-Classification)
  #join_lables()
  # spread(Commodity.Code, Value) %>%
  # mutate(Share_WTO_in_TOTAL = WTO_AgriFood / TOTAL) %>%
  # gather(Commodity.Code, Value, TOTAL, WTO_AgriFood, Share_WTO_in_TOTAL)

df %>% write.csv(str_c("policyBrief/tradeByRegionsLong", Sys.Date(), ".csv"), row.names = FALSE, na = "0")

save(df, file = "policyBrief/appDF.RData")

# agregating values into group by years
