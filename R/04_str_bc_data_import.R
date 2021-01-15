#### 04 STR BC DATA IMPORT #####################################################

#' This script is time-consuming to run, so it should only be rerun when STR 
#' data needs to be rebuilt from scratch.
#' 
#' Output:
#' - `str_bc_raw.qs`
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' 
#' External dependencies:
#' - Access to the UPGo database
#' - Old versions of the AirDNA property file to fix problems with last scraped
#'   dates

source("R/01_startup.R")
qload("output/geometry.qsm", nthreads = availableCores())


# Get data ----------------------------------------------------------------

upgo_connect(daily_inactive = TRUE)

property_bc <- 
  property_remote %>% 
  filter(country == "Canada", region == "British Columbia") %>% 
  collect() %>% 
  filter(!property_ID %in% property$property_ID) %>% 
  strr_as_sf(32610) %>% 
  st_filter(CMA)

daily_bc <- 
  daily_remote %>% 
  filter(property_ID %in% !!property_bc$property_ID) %>% 
  collect() %>% 
  strr_expand()

daily_inactive_bc <- 
  daily_inactive_remote %>% 
  filter(property_ID %in% !!property_bc$property_ID) %>% 
  collect() %>% 
  strr_expand()

host_bc <- 
  host_remote %>% 
  filter(host_ID %in% !!property_bc$host_ID) %>% 
  collect() %>% 
  strr_expand()

upgo_disconnect()


# Manually fix wonky created dates ----------------------------------------

property_bc <- 
  property_bc %>% 
  mutate(created = if_else(is.na(created), first_active, created),
         scraped = if_else(is.na(scraped), last_active, scraped)) %>% 
  filter(!is.na(created))


# Manually fix January scraped date issue ---------------------------------

# Load old property files
prop_04 <- 
  qread(paste0("~/Documents/Academic/Code/global-file-import/", 
               "output/property/property_2020_04.qs"),
        nthreads = availableCores()) %>% 
  select(property_ID, old_scraped = scraped)

# Get fixes
jan_fix_bc <- 
  property_bc %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-29", scraped <= "2020-01-31") %>% 
  left_join(prop_04) %>% 
  filter(scraped < old_scraped) %>% 
  select(property_ID, old_scraped)

# Change scraped date in property file
property_bc <- 
  property_bc %>% 
  left_join(jan_fix_bc) %>% 
  mutate(scraped = if_else(is.na(old_scraped), scraped, old_scraped)) %>% 
  select(-old_scraped)

# Scrape fixed listings with May scraped date to see which are still active
to_scrape_bc <- jan_fix_bc %>% filter(old_scraped >= "2020-05-01")
upgo_scrape_connect()
new_scrape_bc <- 
  to_scrape_bc %>% upgo_scrape_ab(proxies = .proxy_list, cores = 10)
upgo_scrape_disconnect()
still_active_bc <- new_scrape_bc %>% filter(!is.na(country))

# Update scraped dates for active listings
property_bc <- 
  property_bc %>% 
  mutate(scraped = if_else(property_ID %in% still_active_bc$property_ID,
                           as.Date("2020-09-30"), scraped))

# Get inactives
inactives_bc <-
  daily_inactive_bc %>% 
  filter(property_ID %in% jan_fix_bc$property_ID)

# Add inactive rows to daily file
daily_bc <- 
  inactives_bc %>% 
  left_join(select(st_drop_geometry(property_bc), 
                   property_ID, created, scraped)) %>%
  filter(date >= created, date <= scraped) %>%
  select(-created, -scraped) %>% 
  bind_rows(daily_bc)

rm(prop_04, jan_fix_bc, to_scrape_bc, new_scrape_bc, still_active_bc, 
   inactives_bc, daily_inactive_bc)


# Convert currency --------------------------------------------------------

exchange_rates <- 
  convert_currency(start_date = min(daily$date), 
                   end_date = max(daily$date))

daily_bc <- 
  daily_bc %>% 
  mutate(year_month = substr(date, 1, 7)) %>% 
  left_join(exchange_rates) %>% 
  mutate(price = price * exchange_rate) %>% 
  select(-year_month, -exchange_rate)


# Process the property and daily files ------------------------------------

DA_CMA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CMA = "59933"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32610) %>% 
  select(GeoUID, Dwellings) %>% 
  set_names(c("GeoUID", "dwellings", "geometry")) %>% 
  st_set_agr("constant")

# # Run raffle to assign a DA to each listing
# property_bc <-
#   property_bc %>% 
#   strr_raffle(DA_CMA, GeoUID, dwellings, seed = 1)

# Add area to property file
property_bc <-
  property_bc %>%
  st_join(select(CMA, name)) %>% 
  rename(CSD = name)

# Add area to daily file
daily_bc <-
  property_bc %>%
  st_drop_geometry() %>%
  select(property_ID, CSD) %>%
  left_join(daily_bc, ., by = "property_ID")


# Save output -------------------------------------------------------------

qsavem(property_bc, daily_bc, host_bc, file = "output/str_bc_raw.qsm",
       nthreads = availableCores())
