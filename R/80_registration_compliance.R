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

source("R/01_startup.R")

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
load("output/registration.Rdata")

upgo_connect(registration = TRUE)

registration_remote

### 4.1  ----------------------

# how many listings removed by Airbnb in August 2018
property %>%  
  filter(scraped >= key_date_regulations, scraped <= "2018-09-01") %>% 
  nrow() %>% 
  round(digits = -1)
# We did not use that number because it does not take in account the natural
# turnover of airbnb listings. instead we used the number given by Airbnb

# Daily counts of displayed listings and licenses
active_listings <- 
  daily %>% 
  filter(housing) %>% 
  count(date) %>% 
  rename(listings = n)

active_licenses <- 
  BL_expanded %>% 
  count(date) %>% 
  rename(licenses = n)

licenses_listings <- 
  left_join(filter(active_listings, date >= min(active_licenses$date)), 
            filter(active_licenses, date <= max(active_listings$date))) %>% 
  mutate(licenses = slide_dbl(licenses, mean, .before = 6, .complete = TRUE),
         listings = slide_dbl(listings, mean, .before = 6, 
                              .complete = TRUE)) %>% 
  filter(!is.na(licenses)) %>% 
  pivot_longer(-date)

licenses_listings_pct <- 
  licenses_listings %>% 
  group_by(date) %>% 
  mutate(value = if_else(name == "licenses", 
                         value[name == "licenses"] / 
                           value[name == "listings"], 1)) %>% 
  ungroup()

licenses_listings %>% 
  ggplot() +
  geom_col(aes(date, listings), color = col_palette[c(3)]) +
  geom_col(aes(date, licenses), color = col_palette[c(5)])+
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = 0, yend = Inf, alpha = 0.3) +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = 0, yend = Inf, alpha = 0.3) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", #family = "Futura"
        ),
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, #family = "Futura"
        ))+
  ggtitle("All listings (pink) and valid licenses (green), 7 day-average")


# New licenses in fall/winter 2019
pull(filter(active_licenses, date == "2019-12-31"))-
  pull(filter(active_licenses, date == "2019-10-01"))

# Decrease of displyaed listings in fall/winter 2019
pull(filter(active_listings, date == "2019-12-31"))-
  pull(filter(active_listings, date == "2019-10-01"))

# New licenses after COVID
pull(filter(active_licenses, date == max(date)))-
  pull(filter(active_licenses, date == key_date_covid))


### Class by conformity --------------------------------------------------------

# Prepare a conformity vector
conformity_status <- 
  property %>% 
  filter(!is.na(ab_property)) %>% 
  left_join(select(BL, registration, issued, expired), by = "registration") %>% 
  filter(!is.na(ab_property)) %>% 
  mutate(registration_analyzed = 
           ifelse(str_detect(registration, "\\d{2}-\\d{6}") & is.na(issued),
                  "Fake license", "TBD"),
         registration_analyzed = 
           ifelse(str_detect(registration, "\\d{2}-\\d{6}") & 
                    !is.na(issued) & expired < "2020-10-13", 
                  "Expired license", registration_analyzed),
         registration_analyzed = 
           ifelse(str_detect(registration, "\\d{2}-\\d{6}") & 
                    !is.na(issued) & expired > "2020-10-13",
                  "Conform", registration_analyzed),
         registration_analyzed = 
           ifelse(registration == "NO LISTING", "Inactive listing", 
                  registration_analyzed),
         registration_analyzed = 
           ifelse(registration == "EXEMPT", "Exempt", registration_analyzed),
         registration_analyzed = 
           ifelse(registration == "INVALID", "Invalid", registration_analyzed),
         registration_analyzed = ifelse(is.na(registration), "No license", 
                                        registration_analyzed)) %>% 
  select(property_ID, listing_type, created, scraped, active, 
         registration_analyzed)

# Graphing the conformity status of active listings
conformity_status %>% 
  filter(active >= max(active, na.rm = T) - days(30),
         registration_analyzed != "Inactive listing") %>% 
  ggplot()+
  geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
  xlab("")+
  ylab("Number of listings")+
  guides(x = guide_axis(angle = 10))+
  # scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 1, 2, 3, 6)])+
  theme_minimal()

# percentage of all conformity status category for active listings
conformity_status %>% 
  st_drop_geometry() %>% 
  filter(registration_analyzed != "Inactive listing",
         active >= max(active, na.rm = T) - days(30)) %>% 
  count(registration_analyzed) %>% 
  mutate(per = str_glue("{round(n/sum(n), digits = 3)*100}%"))

# How many of the conform listings use a license used by multiple listings
license_scrape %>% 
  st_drop_geometry() %>% 
  filter(license_status == "Valid", active >= "2020-09-01") %>% 
  filter(listing_type == "Entire home/apt") %>% 
  count(registration, sort = TRUE) %>% 
  filter(n > 1) %>%
  pull(n) %>% 
  sum()


# ## Unsure if relevant
# # Graphing the conformity status of displayed listings
# conformity_status %>%
#   filter(registration_analyzed != "Inactive listing") %>%
#   ggplot()+
#   geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
#   xlab("")+
#   ylab("Number of listings")+
#   guides(x = guide_axis(angle = 10))+
#   # scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 1, 2, 3, 6)])+
#   theme_minimal()
# 
# 
# # percentage of all conformity status category for displayed listings
# conformity_status %>%
#   st_drop_geometry() %>%
#   filter(registration_analyzed != "Inactive listing") %>%
#   count(registration_analyzed) %>%
#   mutate(per = n/sum(n))


### Geography of conformity status -------------------------------------------

# percentage of non-conform listings per area

LA %>% 
  st_join(license_scrape) %>% 
  filter(active >= "2020-09-01", license_status != "Exempt") %>% 
  count(area, valid = license_status == "Valid") %>% 
  group_by(area) %>% 
  summarize(invalid = n[!valid], invalid_pct = n[!valid] / sum(n)) %>% 
  ggplot() +
  geom_sf(aes(fill = invalid_pct), colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(6, 1)], na.value = "grey80",
                       labels = scales::percent)  +
  geom_sf_text(aes(label = invalid), colour = "black") +
  guides(fill = guide_colourbar(title = "Non-conforming listings")) + 
  theme_void() +
  theme(legend.position = "bottom")


LA %>% 
  st_join(conformity_status) %>% 
  filter(registration_analyzed != "Inactive listing",
         active >= max(active, na.rm = T) - days(30)) %>% 
  mutate(registration_analyzed = ifelse(registration_analyzed != "Conform",
                                        "Non-conform", registration_analyzed)) %>% 
  count(area.x, registration_analyzed) %>% 
  group_by(area.x) %>% 
  mutate(non_conform_per = n/sum(n)) %>% 
  arrange(desc(non_conform_per)) %>% 
  ungroup() %>% 
  slice(1:3)



### Non-conformity lucrativity table ------------------------------------------
eh_license <-
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(active >= "2020-09-01") %>% 
  filter(listing_type == "Entire home/apt") %>%
  select(property_ID, license_status)

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
  left_join(st_drop_geometry(select(license_scrape, property_ID, created)), 
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
  filter(status == "R", date >= key_date_regulations,
         property_ID %in% eh_license$property_ID) %>%
  group_by(property_ID) %>% 
  summarize(revenue = sum(price)) %>% 
  left_join(st_drop_geometry(select(license_scrape, property_ID, created)), 
            by = "property_ID") %>% 
  mutate(created_or_regulations = 
           as.Date(if_else(created <= key_date_regulations, 
                           key_date_regulations, created), 
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
  filter(license_status != "Valid", license_status != "Exempt") %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(license_status = "Non-conforming")

all_listings <- 
  license_activity %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(license_status = "All listings")

license_activity <-
  all_listings %>% 
  bind_rows(license_activity %>% 
              group_by(license_status) %>%
              summarize(n = n(), across(per_A:revenue_reg, mean))) %>% 
  bind_rows(invalid) %>% 
  select(license_status, everything())

conformity_table %>% 
  set_names(c("Conformity status", "Number of listings", "Available", 
              "Reserved", "Blocked", "Revenue per night since COVID-19", 
              "Revenue per night since regulations")) %>% 
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

