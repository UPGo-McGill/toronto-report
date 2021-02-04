#### 13 NATIONAL COMPARISON ####################################################

#' This script is time-consuming to run, so it should only be rerun when STR
#' data needs to be rebuilt from scratch.
#'
#' Output:
#' - `national_comparison.qs`
#'
#' Script dependencies:
#' - `13_FREH_model.R`
#'
#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")

# Load data ---------------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())
load("output/national_comparison.Rdata")

national_comparison_2019 <- 
  national_comparison %>% 
  rename(active_daily_listings_2019=active_daily_listings,
         revenue_2019=revenue) %>% 
  select(-listings_per_1000, -revenue_per_listing)

rm(national_comparison)

# Get geometries for 10 biggest cities ------------------------------------

CSD <-
  cancensus::get_census("CA16", list(C = "01"), level = "CSD",
                        geo_format = "sf") %>%
  arrange(-Population) %>%
  slice(1:10) %>%
  mutate(name = str_extract(name, '.*(?= )'),
         name = stringi::stri_trans_general(name, "Latin-ASCII")) %>%
  as_tibble() %>%
  st_as_sf()


# Get STR data for same cities --------------------------------------------

upgo_connect()

property_CA <-
  property_remote %>%
  filter(country == "Canada", city %in% !!CSD$name) %>%
  collect() %>%
  strr_as_sf() %>%
  st_filter(CSD)

daily_CA <-
  daily_remote %>%
  filter(property_ID %in% !!property_CA$property_ID, year(start_date) == 2020) %>%
  collect() %>%
  strr_expand() %>%
  # Reconcile geography inconsistencies between property and daily files
  select(-country, -region, -city) %>%
  left_join(select(st_drop_geometry(property_CA), property_ID, country:city))

upgo_disconnect()


# Replace Toronto data with processed results -----------------------------

property_CA <-
  property_CA %>%
  filter(city != "Toronto") %>%
  select(-first_active, -last_active) %>%
  rbind(select(st_transform(property, 4326), -c(GeoUID:all_PIDs, active)))

daily_CA <-
  daily_CA %>%
  filter(city != "Toronto") %>%
  rbind(select(daily, -c(ward:FREH_3))) %>%
  filter(year(date) == 2020)


# Calculate figures -------------------------------------------------------

national_comparison <-
  daily_CA %>%
  filter(status != "B", housing) %>%
  group_by(city) %>%
  summarize(active_daily_listings = n() / 366, .groups = "drop") %>%
  left_join(select(st_drop_geometry(CSD), name, Dwellings),
            by = c("city" = "name")) %>%
  mutate(listings_per_1000 = 1000 * active_daily_listings / Dwellings)

exchange_rates <- convert_currency(start_date = "2020-01-01",
                                   end_date = "2020-12-31")

national_comparison <-
  daily_CA %>%
  filter(status == "R", housing) %>%
  mutate(year_month = substr(date, 1, 7)) %>%
  left_join(exchange_rates) %>%
  mutate(price = price * exchange_rate) %>%
  select(-year_month, -exchange_rate) %>%
  group_by(city) %>%
  summarize(revenue = sum(price)) %>%
  left_join(national_comparison, .)

national_comparison <-
  national_comparison %>%
  mutate(revenue_per_listing = revenue / active_daily_listings)

national_comparison <- 
  national_comparison %>% 
  left_join(., national_comparison_2019, by = "city") %>% 
  mutate(YOY_active_listings = (active_daily_listings-active_daily_listings_2019)/active_daily_listings_2019,
         YOY_revenue = (revenue-revenue_2019)/revenue_2019) %>% 
  select(-revenue_2019, -active_daily_listings_2019)


# Save output -------------------------------------------------------------

qsave(national_comparison, file = "output/national_comparison.qs",
      nthreads = availableCores())
