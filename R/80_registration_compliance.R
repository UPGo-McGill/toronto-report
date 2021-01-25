#### 80 CHAPTER X ANALYSIS & GRAPHS ##########################################

#' This script produces the tables and facts for chapter X. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `09_str_processing.R`
#' - `registration.R` # dont know yet which script it is or will be in
#' 
#' External dependencies:
#' - None

#upgo_connect(registration = TRUE)

#registration_remote

source("R/01_startup.R")

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/reg_3.qs", nthreads = availableCores())
#qload("output/FREH_model.qsm", nthreads = availableCores())

# Prepare new objects -----------------------------------------------------

# Upload raw ward data
WD_raw <- read_sf("data/wards/City Wards Data.shp") %>% 
  select(FIELD_11, FIELD_13) %>% 
  rename(ward_number = FIELD_11, ward = FIELD_13) %>% 
  st_drop_geometry()

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

# Join open data's scrape with Airbnb scrape
reg <- left_join(reg_3, str_reg, by = "registration")

# Select property_IDs that are using duplicate registration numbers
duplicates <- 
  reg %>% 
  count(registration) %>% 
  filter(n>=2, n<=274) %>% 
  pull(registration)

# Categorize registration licences by type
# First, change the formatting
reg <- 
  reg %>% 
  mutate(registration = toupper(registration),
         registration = case_when(
           is.na(registration) ~ NA_character_,
           registration == "NO LISTING" ~ "NO LISTING",
           registration == "EXEMPT" ~ "EXEMPT",
           str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
           TRUE ~ "INVALID")
  ) 

# Make sure that duplicates are entered
reg$registration_analyzed <- ifelse(reg$registration %in% duplicates, "Duplicates", reg$registration)

# Categorize the rest
reg <- 
  reg %>% 
  mutate(registration_analyzed = ifelse(registration == "NO LISTING", "Inactive listing", registration_analyzed),
         registration_analyzed = ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration, "STR-\\d{4}-\\w{6}") & is.na(id), 
                                        "Fake License", registration_analyzed),
         registration_analyzed = ifelse(str_detect(registration_analyzed, "STR-\\d{4}-\\w{6}"), 
                                        "Conform", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration_analyzed), "No license", registration_analyzed)
  ) %>% 
  as_tibble()


# Count of registration compliance for displayed listings
reg %>% 
  group_by(registration_analyzed) %>% 
  summarize(n = n(), percentage = n/nrow(reg)*100) 

# Add property information of listings scraped in january
january_listings <- 
  reg %>% 
  select(property_ID, registration, registration_analyzed, postal_code) %>% 
  left_join(., property, by = "property_ID") %>% 
  st_as_sf()

# Percentage of inactive listings 
january_listings %>% 
  st_drop_geometry() %>% 
  group_by(active = active >= max(active, na.rm = T) - days(30)) %>% 
  summarize(percentage_active = n()/nrow(january_listings)) %>% 
  mutate(percentage_active = str_glue("{round(percentage_active, digits = 3)*100}%"))


### Class by conformity --------------------------------------------------------

# percentage of all conformity status category for active listings only
january_listings %>% 
  st_drop_geometry() %>% 
  filter(registration_analyzed != "Inactive listing",
         active >= max(active, na.rm = T) - days(30)) %>% 
  count(registration_analyzed) %>% 
  mutate(per = str_glue("{round(n/sum(n), digits = 3)*100}%"))


## Unsure if relevant
# Graphing the conformity status of displayed listings
january_listings %>%
   filter(registration_analyzed != "Inactive listing", registration_analyzed != "Invalid") %>%
   ggplot()+
   geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
   xlab("")+
   ylab("Number of listings")+
   guides(x = guide_axis(angle = 10))+
   scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 6, 2, 3, 1)])+
   theme_minimal()

# Graphing the conformity status of active listings
january_listings %>%
   filter(registration_analyzed != "Inactive listing", registration_analyzed != "Invalid",
          active >= max(active, na.rm = T) - days(30)) %>%
   ggplot()+
   geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
   xlab("")+
   ylab("Number of listings")+
   guides(x = guide_axis(angle = 10))+
   scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 6, 2, 3, 1)])+
   theme_minimal()
# 
# 
# # percentage of all conformity status category for displayed listings
# conformity_status %>%
#   st_drop_geometry() %>%
#   filter(registration_analyzed != "Inactive listing") %>%
#   count(registration_analyzed) %>%
#   mutate(per = n/sum(n))


### Geography of conformity status -------------------------------------------

# percentage and number of non-conform active listings per area
WD %>% 
  st_join(january_listings %>% select(-ward)) %>% 
  filter(active >= max(active, na.rm = T) - days(30), 
         registration_analyzed != "Exempt") %>% 
  count(ward, valid = registration_analyzed == "Conform") %>% 
  group_by(ward) %>% 
  summarize(invalid = n[!valid], invalid_pct = n[!valid] / sum(n)) %>% 
  ggplot() +
  geom_sf(aes(fill = invalid_pct), colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3, 4, 5)], na.value = "grey80",
                       oob = scales::squish, 
                       labels = scales::percent_format(accuracy = 1))  +
  geom_sf_text(aes(label = invalid), colour = "black") +
  guides(fill = guide_colourbar(title = "Non-conforming listings")) + 
  theme_void() +
  theme(legend.position = "bottom")


### Non-conformity lucrativity table ------------------------------------------
eh_license <-
  january_listings %>% 
  st_drop_geometry() %>% 
  filter(active >= max(active, na.rm = T) - days(30)) %>% 
  filter(listing_type == "Entire home/apt") %>%
  select(property_ID, registration_analyzed)

pr_license <-
  january_listings %>% 
  st_drop_geometry() %>% 
  filter(active >= max(active, na.rm = T) - days(30)) %>% 
  filter(listing_type == "Private room") %>%
  select(property_ID, registration_analyzed)

daily_status <-
  daily %>% 
  filter(date >= key_date_covid, property_ID %in% eh_license$property_ID) %>%
  group_by(property_ID) %>% 
  count(status) %>% 
  mutate(sum_status = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("property_ID", "sum_status"), names_from = "status", 
              values_from = "n") %>% 
  mutate(A = ifelse(is.na(A), 0, A),
         R = ifelse(is.na(R), 0, R),
         B = ifelse(is.na(B), 0, B),
         per_A = A / sum_status,
         per_R = R / sum_status,
         per_B = B / sum_status) %>% 
  select(-sum_status, -A, -R, -B)

license_activity <- inner_join(eh_license, daily_status, by = "property_ID")

revenue_covid <-
  daily %>% 
  filter(status == "R", date >= key_date_covid,
         property_ID %in% eh_license$property_ID) %>%
  group_by(property_ID) %>% 
  summarize(revenue = sum(price)) %>% 
  left_join(st_drop_geometry(select(january_listings, property_ID, created)), 
            by = "property_ID") %>% 
  mutate(created_or_covid = 
           as.Date(if_else(created <= key_date_covid, key_date_covid, created), 
                   origin = "1970-01-01")) %>% 
  mutate(revenue = revenue / as.numeric(max(daily$date) - created_or_covid)) %>% 
  filter(revenue != Inf) %>% 
  select(property_ID, revenue)
  
license_activity <- 
  inner_join(license_activity, revenue_covid, by = "property_ID") %>% 
  mutate(revenue = if_else(is.na(revenue), 0, revenue))

revenue_regulations <-
  daily %>% 
  filter(status == "R", date >= start_2019,
         property_ID %in% eh_license$property_ID) %>%
  group_by(property_ID) %>% 
  summarize(revenue = sum(price)) %>% 
  left_join(st_drop_geometry(select(january_listings, property_ID, created)), 
            by = "property_ID") %>% 
  mutate(created_or_regulations = 
           as.Date(if_else(created <= start_2019, 
                           start_2019, created), 
                   origin = "1970-01-01")) %>% 
  mutate(revenue = revenue / as.numeric(max(daily$date) - 
                                          created_or_regulations)) %>% 
  filter(revenue != Inf) %>% 
  select(property_ID, revenue_reg = revenue)

license_activity <- 
  inner_join(license_activity, revenue_regulations, by = "property_ID") %>% 
  mutate(revenue_reg = if_else(is.na(revenue_reg), 0, revenue_reg))

invalid <- 
  license_activity %>% 
  filter(registration_analyzed != "Conform", registration_analyzed != "Exempt") %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(registration_analyzed = "Non-conforming")

all_listings <- 
  license_activity %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(license_status = "All listings")

license_activity <-
  all_listings %>% 
  bind_rows(license_activity %>% 
              group_by(registration_analyzed) %>%
              summarize(n = n(), across(per_A:revenue_reg, mean))) %>% 
  bind_rows(invalid) %>% 
  select(registration_analyzed, everything())

license_activity %>% 
  select(-license_status) %>% 
  set_names(c("Conformity status", "Number of listings", "Available", 
              "Reserved", "Blocked", "Revenue per night since COVID-19", 
              "Revenue per night since start of 2019")) %>% 
  gt() %>% 
  fmt_currency(
    columns = 6:7,
    currency = "CAD"
  ) %>% 
  fmt_percent(
    columns = 3:5, 
    decimals = 1)



### Commercial listings compliance ---------------------------------

commercial_2020 <- 
  daily %>% 
  filter(date >= "2020-01-01", # because current registration IDs starts at beginning of year
         FREH_3 >= 0.5 | multi) %>% 
  pull(property_ID) %>% unique()

conformity_status %>% 
  filter(property_ID %in% commercial_2020, 
         active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% 
  nrow()

conformity_status %>% 
  filter(property_ID %in% commercial_2020, 
         active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% nrow()/
  conformity_status %>% 
  filter(active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% nrow()

conformity_status %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% commercial_2020, 
         active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% 
  count(registration_analyzed) %>% 
  mutate(per = str_glue("{round(n/sum(n), digits = 3)*100}%"))

conformity_status %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% commercial_2020, 
         active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% 
  ggplot()+
  geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
  xlab("")+
  ylab("Number of listings")+
  guides(x = guide_axis(angle = 10))+
  # scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 1, 2, 3, 6)])+
  theme_minimal()







# New table ---------------------------------------------------------------

# modification of daily to join with the new df with commercial status
daily_FREH_09 <- 
  daily %>% 
  filter(property_ID %in% filter(conformity_status, 
                                 registration_analyzed != 
                                   "Inactive listing")$property_ID) %>% 
  select(property_ID, date, FREH_3) %>% 
  filter(date >= "2020-09-01") %>% 
  arrange(desc(FREH_3)) %>% 
  mutate(FREH = ifelse(FREH_3 >= 0.5, TRUE, FALSE)) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  select(property_ID, FREH) %>% 
  distinct()

daily_multi_09 <- 
  daily %>% 
  filter(property_ID %in% filter(conformity_status, 
                                 registration_analyzed != 
                                   "Inactive listing")$property_ID) %>% 
  select(property_ID, date, multi) %>% 
  filter(date >= "2020-09-01") %>% 
  arrange(desc(multi)) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  select(property_ID, multi) %>% 
  distinct()

daily_scraped_in_september <- 
  inner_join(daily_FREH_09, daily_multi_09)

# Creation of a new df for improved graph
conformity_status_commercial_09 <- 
  conformity_status %>% 
  st_drop_geometry() %>% 
  inner_join(daily_scraped_in_september, by = "property_ID") %>% 
  select(property_ID, active, registration_analyzed, FREH, multi) %>% 
  mutate(fill = ifelse(active >= "2020-09-01" & FREH == T, "FREH", "TBD"),
         fill = ifelse(active >= "2020-09-01" & FREH == F & multi == T, "ML", 
                       fill),
         fill = ifelse(active >= "2020-09-01" & FREH == F & multi == F, 
                       "Non-commercial", fill),
         fill = ifelse(active < "2020-09-01" | is.na(active), "Inactive", fill))

# plotting them
conformity_status_commercial_09 %>% 
  ggplot() +
  geom_histogram(stat = "count", aes(registration_analyzed, fill = fill))+
  # geom_bar(position = "fill", stat = "count", aes(registration_analyzed, fill = fill)) +
  xlab("") +
  ylab("Number of listings") +
  guides(x = guide_axis(angle = 10)) +
  scale_fill_manual(name = "Listing type", values = col_palette[c(4, 2, 1, 6)]) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

