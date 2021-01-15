#### 05 STR CMA DATA IMPORT ####################################################

#' This script is time-consuming to run, so it should only be rerun when STR 
#' data needs to be rebuilt from scratch.
#' 
#' Output:
#' - `cma_comparison.qsm`
#' 
#' Script dependencies:
#' - None
#' 
#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")
library(cancensus)

upgo_connect()


# Get Montreal and Toronto CMAs -------------------------------------------

CMA_montreal <- 
  get_census("CA16", list(CMA = "24462"), "CSD", geo_format = "sf") %>% 
  st_transform(32618)

CMA_toronto <- 
  get_census("CA16", list(CMA = "35535"), "CSD", geo_format = "sf") %>% 
  st_transform(32617)

property_montreal <- 
  property_remote %>% 
  filter(country == "Canada", region == "Québec") %>% 
  collect() %>% 
  strr_as_sf(32618) %>% 
  st_filter(CMA_montreal)

property_montreal <- 
  property_montreal %>% 
  mutate(central = as.logical(st_intersects(
    geometry, 
    CMA_montreal[CMA_montreal$name == "Montréal (V)",]$geometry))) %>% 
  mutate(central = if_else(is.na(central), FALSE, central))

property_toronto <- 
  property_remote %>% 
  filter(country == "Canada", region == "Ontario") %>% 
  collect() %>% 
  strr_as_sf(32617) %>% 
  st_filter(CMA_toronto)

property_toronto <- 
  property_toronto %>% 
  mutate(central = as.logical(st_intersects(
    geometry, 
    CMA_toronto[CMA_toronto$name == "Toronto (C)",]$geometry))) %>% 
  mutate(central = if_else(is.na(central), FALSE, central))

daily_montreal <- 
  daily_remote %>% 
  filter(property_ID %in% !!property_montreal$property_ID) %>% 
  collect() %>% 
  strr_expand()

daily_montreal <- 
  property_montreal %>% 
  st_drop_geometry() %>% 
  select(property_ID, central) %>% 
  left_join(daily_montreal, .)

daily_toronto <- 
  daily_remote %>% 
  filter(property_ID %in% !!property_toronto$property_ID) %>% 
  collect() %>% 
  strr_expand()

daily_toronto <- 
  property_toronto %>% 
  st_drop_geometry() %>% 
  select(property_ID, central) %>% 
  left_join(daily_toronto, .)

host_montreal <- 
  host_remote %>% 
  filter(host_ID %in% !!property_montreal$host_ID) %>% 
  collect() %>% 
  strr_expand()

host_toronto <- 
  host_remote %>% 
  filter(host_ID %in% !!property_toronto$host_ID) %>% 
  collect() %>% 
  strr_expand()

daily_montreal <- 
  daily_montreal %>% 
  strr_multi(host_montreal)

daily_toronto <- 
  daily_toronto %>% 
  strr_multi(host_toronto)

upgo_disconnect()


# Save output -------------------------------------------------------------

qsavem(property_montreal, property_toronto, daily_montreal, daily_toronto,
       host_montreal, host_toronto, file = "output/cma_comparison.qsm", 
       nthreads = availableCores())
