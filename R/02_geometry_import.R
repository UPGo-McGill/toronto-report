#### 02 GEOMETRY IMPORT ########################################################

#' This script should only be rerun when geometry needs to be rebuilt from
#' scratch.
#'
#' Output:
#' - `geometry.qsm`
#'
#' Script dependencies:
#' - None
#'
#' External dependencies:
#' - `local-area-boundary.shp`: Shapefile of Vancouver's local areas

source("R/01_startup.R")
library(cancensus)
library(osmdata)


# ON province -------------------------------------------------------------

province <-
  get_census("CA16", regions = list(PR = "35"), geo_format = "sf") %>%
  st_transform(32617) %>%
  select(geometry)


# Toronto CMA without Toronto ---------------------------------------------

CMA <-
  get_census(
    dataset = "CA16", regions = list(CMA = "35535"), level = "CSD",
    geo_format = "sf") %>%
  filter(name != "Toronto (C)") %>%
  st_transform(32617)


# Toronto CSD -------------------------------------------------------------

city <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3520005"), geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# Toronto DAs -------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3520005"), level = "DA",
    geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# Toronto neighbourhoods --------------------------------------------------

WD_raw <- read_sf("data/wards/City Wards Data.shp") %>% 
  select(FIELD_11, FIELD_13) %>% 
  rename(ward_number = FIELD_11, ward = FIELD_13) %>% 
  st_drop_geometry()

WD <-
  read_sf("data/wards/City Wards Data.shp") %>%
  select(ward = FIELD_13) %>%
  st_set_agr("constant") %>%
  st_as_sf() %>%
  st_transform(32617) %>%
  st_intersection(province)

WD <-
  DA %>%
  select(dwellings) %>%
  st_interpolate_aw(WD, extensive = TRUE) %>%
  st_drop_geometry() %>%
  select(dwellings) %>%
  cbind(WD, .) %>%
  as_tibble() %>%
  st_as_sf() %>%
  arrange(ward)


# Streets -----------------------------------------------------------------

#streets <-
#  (getbb("Toronto") * c(1.01, 0.99, 0.99, 1.01)) %>%
#  opq(timeout = 200) %>%
#  add_osm_feature(key = "highway") %>%
#  osmdata_sf()

#streets <-
#  rbind(
#    streets$osm_polygons %>% st_set_agr("constant") %>% st_cast("LINESTRING"),
#    streets$osm_lines) %>%
#  as_tibble() %>%
#  st_as_sf() %>%
#  st_transform(32617) %>%
#  st_set_agr("constant") %>%
#  st_intersection(city)

#streets <-
#  streets %>%
#  filter(highway %in% c("primary", "secondary")) %>%
#  select(osm_id, name, highway, geometry)


# Business licenses ------------------------------------------------------

qload("output/reg_1.qs", nthreads = availableCores())
reg_1 <- data.frame(property_ID, date, registration)  %>% 
  as_tibble() %>% 
  filter(registration != "NO LISTING" | is.na(registration)) 

qload("output/reg_2.qs", nthreads = availableCores())
reg_2 <- data.frame(property_ID, date, registration) %>% 
  filter(registration != "NO LISTING" | is.na(registration)) %>% 
  as_tibble()

qload("output/reg_3.qs", nthreads = availableCores())
reg_3 <- data.frame(property_ID, date, registration) %>% 
  filter(registration != "NO LISTING" | is.na(registration)) %>% 
  as_tibble() 

qload("output/reg_4.qs", nthreads = availableCores())
reg_4 <- data.frame(property_ID, date, registration) %>% 
  filter(registration != "NO LISTING" | is.na(registration)) %>% 
  as_tibble() 

qload("output/reg_5.qs", nthreads = availableCores())
reg_5 <- data.frame(property_ID, date, registration) %>% 
  filter(registration != "NO LISTING" | is.na(registration)) %>% 
  as_tibble() 

rm(date, registration, property_ID)

# Upload open data's STR file
str_reg <- read.csv("data/str.csv") %>% 
  rename(registration = operator_registration_number,
         id=X_id) %>% 
  left_join(., WD_raw, by = "ward_number") %>% 
  select(-ward_number)

# Make sure there are no duplicates in this file
(lengths(str_reg))-(str_reg %>% 
                      distinct(registration, .keep_all = TRUE) %>% 
                      lengths())

# Get minimum stay column for properties
property_minimum_stay <-  
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, minimum_stay)


# Join open data's scrape with Airbnb scrape
reg_1 <- left_join(reg_1, str_reg, by = "registration")
reg_2 <- left_join(reg_2, str_reg, by = "registration")
reg_3 <- left_join(reg_3, str_reg, by = "registration")
reg_4 <- left_join(reg_4, str_reg, by = "registration")
reg_5 <- left_join(reg_5, str_reg, by = "registration")

# Join regulation with minimum stay information
reg_1 <- left_join(reg_1, property_minimum_stay, by = "property_ID")
reg_2 <- left_join(reg_2, property_minimum_stay, by = "property_ID")
reg_3 <- left_join(reg_3, property_minimum_stay, by = "property_ID")
reg_4 <- left_join(reg_4, property_minimum_stay, by = "property_ID")
reg_5 <- left_join(reg_5, property_minimum_stay, by = "property_ID")

# Select property_IDs that are using duplicate registration numbers
duplicates_1 <- 
  reg_1 %>% 
  filter(registration != "Exempt", registration != "HOMEAWAY") %>% 
  count(registration) %>% 
  filter(n>=2) %>% 
  pull(registration)

duplicates_2 <- 
  reg_2 %>%
  filter(registration != "Exempt", registration != "HOMEAWAY") %>% 
  count(registration) %>% 
  filter(n>=2) %>% 
  pull(registration)

duplicates_3 <- 
  reg_3 %>%
  filter(registration != "Exempt", registration != "HOMEAWAY") %>% 
  count(registration) %>% 
  filter(n>=2) %>% 
  pull(registration)

duplicates_4 <- 
  reg_4 %>% 
  filter(registration != "Exempt", registration != "HOMEAWAY") %>% 
  count(registration) %>% 
  filter(n>=2) %>% 
  pull(registration)

duplicates_5 <- 
  reg_5 %>% 
  filter(registration != "Exempt", registration != "HOMEAWAY") %>% 
  count(registration) %>% 
  filter(n>=2) %>% 
  pull(registration)

# Categorize registration licences by type
# First, change the formatting
reg_1 <- 
  reg_1 %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  ) 

reg_2 <- 
  reg_2 %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  ) 

reg_3 <- 
  reg_3 %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  )

reg_4 <- 
  reg_4 %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  )

reg_5 <- 
  reg_5 %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  )

# Make sure that duplicates are entered
reg_1$registration_analyzed <- ifelse(reg_1$registration %in% duplicates_1, "Duplicates", reg_1$registration)
reg_2$registration_analyzed <- ifelse(reg_2$registration %in% duplicates_2, "Duplicates", reg_2$registration)
reg_3$registration_analyzed <- ifelse(reg_3$registration %in% duplicates_3, "Duplicates", reg_3$registration)
reg_4$registration_analyzed <- ifelse(reg_4$registration %in% duplicates_4, "Duplicates", reg_4$registration)
reg_5$registration_analyzed <- ifelse(reg_5$registration %in% duplicates_5, "Duplicates", reg_5$registration)

# Categorize the rest
reg_1 <- 
  reg_1 %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed) & minimum_stay >=28, "28-day min.", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>%
  filter(registration_analyzed != "Invalid") %>% 
  as_tibble()

reg_2 <- 
  reg_2 %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed) & minimum_stay >=28, "28-day min.", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>%
  filter(registration_analyzed != "Invalid") %>% 
  as_tibble()

reg_3 <- 
  reg_3 %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed) & minimum_stay >=28, "28-day min.", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>%
  filter(registration_analyzed != "Invalid") %>% 
  as_tibble()

reg_4 <- 
  reg_4 %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed) & minimum_stay >=28, "28-day min.", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>%
  filter(registration_analyzed != "Invalid") %>% 
  as_tibble()

reg_5 <- 
  reg_5 %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed) & minimum_stay >=28, "28-day min.", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>%
  filter(registration_analyzed != "Invalid") %>% 
  as_tibble()


# Save output -------------------------------------------------------------

qsavem(province, CMA, DA, city, WD,
       #streets, BL, BL_expanded, skytrain,
       file = "output/geometry.qsm", nthreads = availableCores())

qsavem(reg_1, reg_2, reg_3, reg_4, reg_5,
       file = "output/regulation.qsm", nthreads = availableCores())
