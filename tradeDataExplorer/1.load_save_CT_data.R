# Eduard Bukin
# Loading data for GEO from the CT data file and saving it in one R_data object

# Setups ------------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
devtools::install_github("EBukin/tradeAnalysis", ref = "pack")
library(tradeAnalysis)

# Loading funcitons
# l_ply(str_c("R/", list.files("R/",pattern="*.R")), source)

# Setuping parallel connection --------------------------------------------

# library(doSNOW)

# Reading GEO data ------------------------------------------------------------

# Georgian Area Code
# geo <- "268"

load("~/01_trade/tradeAnalysis/data/wtoAgFood.Rdata")

countries <- getFSR()$Partner.Code  

# Loading Annual data
anFolder <- "../data/data_raw/ct_zip/a_by_country/"
allAnDataFiles <- listCTdata(anFolder)

# Reading data
ctdata <-
  ddply(
    .data =
      allAnDataFiles %>% 
      filter(r %in% countries,
             year >= 2000),
    .variables = .(name),
    .fun = function(x) {
      wtoAgFoodFull %>% 
        select(Commodity.Code) %>% 
        distinct() %>% 
        left_join(readCTZIP(file = x$name, folder = anFolder), 
                  by = "Commodity.Code")
    },
    .parallel = FALSE,
    .progress = "text"
  ) %>%
  tbl_df() %>%
  select(-name) 

# Normalizing CT data
ctdata <- 
  ctdata %>% 
  norm_ct_data()

# Saving data
save(ctdata, file = "tradeDataExplorer/ctAnWTOFSR.Rdata")

# Loading monthly data
monthFolder <- "../data/data_raw/ct_zip/m_by_country/"
allMonDataFiles <- listCTdata(monthFolder)

# Reading data
ctdata <-
  ddply(
    .data =
      allMonDataFiles %>% 
      filter(r %in% countries,
             year >= 2000),
    .variables = .(name),
    .fun = function(x) {
      readCTZIP(file = x$name, folder = monthFolder)
    },
    .parallel = FALSE,
    .progress = "text"
  ) %>%
  tbl_df() %>%
  select(-name) %>% 
  norm_ct_data()

# Saving data -------------------------------------------------------------

save(ctdata, file = "tradeDataExplorer/ctMonFSR1.Rdata")


