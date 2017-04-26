# Eduard Bukin
# Loading all CT data related to AG in small R data files one per year 
#     Data is loaded for each year and all countries per year. 
#     Data includes only raw deepest level disaggregated commodity codes at the WTO Agrifuud classification.

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

# Loading WTO commodity codes.
load("~/01_trade/tradeAnalysis/data/wtoAgFood.Rdata")

# Data source folder
fromFolder <- "../data/data_raw/ct_zip/a_all/"

# Data destination folder
toFolder <- "data_raw/ctAnAll/"

# WTO filtered data destination folder
toWTOFolder <- "data_raw/wtoAnAll/"

# Years for data reloadign and rereading 
years <- c(1995:2016)


# Resaving all data -----------------------------------------------------------

# # listing all available data
# allAnDataFiles <- listCTdata(fromFolder)
# 
# # Reloading and resaving some data
# d_ply(listCTdata(fromFolder) %>% filter(ps %in% years),
#       .(name),
#       function(x) {
#         gc()
#         object <- str_c("ctAnAll_",x$ps)
#         assign(object, readCTZIP(file = x$name, folder = fromFolder))
#         save(list = c(object), file = file.path(toFolder, str_c(object, "_", Sys.Date(),".Rdata")))
#         rm(list = object)
#         gc()
#       },
#       .progress = "text")


# Loading all CT data and filtering WTO commodities -----------------------

wtoCommodities <- wtoAgFoodFull$Commodity.Code %>% unique() %>% c(., "TOTAL")

# Function loads only the most recent files
allCTFiles <-
  tibble(files = list.files(path = toFolder, pattern = "*.Rdata")) %>% 
  separate(files, c("name", "Year", "Loaded"), sep = "_", remove = FALSE) %>% 
  separate(Loaded, c("Loaded", "ext"), sep = "\\.") %>%
  group_by(Year) %>% 
  arrange(Year, Loaded) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  mutate(files = file.path(toFolder, files))

# Filtering and resaving WTO data
d_ply(allCTFiles %>%
        filter(Year %in% years) ,
      .(files),
      
      function(x) {
        gc(verbose = FALSE)
        
        # Loading unsorted data
        loadedObject <- load(file = x$files, verbose = F)
        
        # Destination object
        objectName <- str_c("wtoAnAll_", x$Year)
        
        # Filtering WTO specific data
        assign(
          objectName,
          eval(parse(text = loadedObject)) %>%
            filter(
              Trade.Flow.Code %in% c(1, 2),
              Commodity.Code %in% wtoCommodities
            ) %>%
            norm_ct_data() %>%
            filter(Variable == "Trade.Value..US..")
        )
        gc()
        
        # Saving one year specific data
        save(list = c(objectName),
             file = file.path(toWTOFolder, str_c(objectName, ".Rdata")))
        
        # Returning data
        rm(list =  c(objectName))
        
        gc()
      }, 
      .progress = "text") 
gc()

# Combining all data to one WTO data file ---------------------------------

wtoAnAll <- 
  llply(tibble(files = file.path(toWTOFolder,list.files(path = toWTOFolder, pattern = "*.Rdata")))$files ,
        function(x) {
          loadedObject <- load(file = x, verbose = F)
          return(eval(parse(text = loadedObject)))
        }) %>% 
  bind_rows

# Using only main WTO HS codes to shorten the dataset.
wtoAnAllMain <- 
  wtoAnAll %>% 
  filter(Commodity.Code %in% c(wtoAgFood$Commodity.Code, "TOTAL"))

# Saving data -------------------------------------------------------------

save(wtoAnAll, file = "data_raw/wtoAnAll.Rdata")


save(wtoAnAllMain, file = "data_raw/wtoAnAllMain.Rdata")


