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
#qload("output/FREH_model.qsm", nthreads = availableCores())



# Count of registration compliance for displayed listings
table_1 <- 
  reg_1 %>% 
  group_by(registration_analyzed) %>% 
  summarize(n=n(), percentage = n()/nrow(reg_2)) %>% 
  mutate(date = as.Date("2021-01-07"))

table_2 <- 
  reg_2 %>% 
  group_by(registration_analyzed) %>% 
  summarize(n=n(), percentage = n()/nrow(reg_2)) %>% 
  mutate(date = as.Date("2021-01-10"))

table_3 <- 
  reg_3 %>% 
  group_by(registration_analyzed) %>% 
  summarize(n=n(), percentage = n()/nrow(reg_3)) %>% 
  mutate(date = as.Date("2021-01-15"))

table_4 <- 
  reg_4 %>% 
  group_by(registration_analyzed) %>% 
  summarize(n=n(), percentage = n()/nrow(reg_4)) %>% 
  mutate(date = as.Date("2021-01-26"))

table <- rbind(table_1, table_2, table_3, table_4)

# total number of active listings per scrape date in jan 2021
table_total <- 
  table %>%
  group_by(date) %>% 
  summarize(n=sum(n))

table_total %>% 
  ggplot()+
  geom_line(aes(x=date, y= n), color=col_palette[5]) + 
  scale_x_date(name=NULL)+
  scale_y_continuous(name="Number of active listings", label = scales::comma)+
  theme_minimal()

# active listings by conformity status in jan 2021
fig_left <- 
  table %>% 
  ggplot()+
  geom_line(aes(x=date, y= percentage, color=registration_analyzed)) +
  scale_colour_manual(name = "Conformity Status", values = col_palette[c(1,2,4,5,6)])+
  scale_x_date(name=NULL)+
  scale_y_continuous(name="Percentage of active listings", label = scales::percent)+
  theme_minimal()

# Number of listings removed between jan 10 and jan 15
listings_removed_jan_15 <- 
  (table_2 %>% filter(registration_analyzed == "No license") %>% pull(n)-
  table_3 %>% filter(registration_analyzed == "No license") %>% pull(n)) %>% 
  round(digit = -1) %>% 
  prettyNum(",")

active_listings_jan_07 <- 
  table_total %>%
  group_by(date) %>% 
  summarize(n=sum(n)) %>% 
  filter(date == "2021-01-07") %>% 
  pull(n) %>% 
  round(digit = -1) %>% 
  prettyNum(",")

active_listings_jan_26 <- 
  table_total %>%
  group_by(date) %>% 
  summarize(n=sum(n)) %>% 
  filter(date == "2021-01-26") %>% 
  pull(n) %>% 
  round(digit = -1) %>% 
  prettyNum(",")

# Percentage listings with valid licenses
conform_listings_jan_7 <- 
  table %>% 
  filter(date == "2021-01-07", registration_analyzed == "Conform") %>% 
  pull(n) %>%
  round(digit = -1) %>% 
  prettyNum(",")

conform_listings_jan_7_perc <- 
  table %>% 
  filter(date == "2021-01-07", registration_analyzed == "Conform") %>% 
  pull(percentage) %>%
  round(3) %>% 
  scales::percent(0.1)

conform_listings_jan_26 <- 
  table %>% 
  filter(date == "2021-01-26", registration_analyzed == "Conform") %>% 
  pull(n) %>%
  round(digit = -1) %>% 
  prettyNum(",")

conform_listings_jan_26_perc <- 
  table %>% 
  filter(date == "2021-01-26", registration_analyzed == "Conform") %>% 
  pull(percentage) %>%
  round(3) %>% 
  scales::percent(0.1)

increase_conform_listings <- 
  ((table %>% filter(date == "2021-01-26", registration_analyzed == "Conform") %>% pull(n))-
  (table %>% filter(date == "2021-01-07", registration_analyzed == "Conform") %>% pull(n))) %>% 
  round(digit = -1) %>% 
  prettyNum(",")
  

### Class by conformity --------------------------------------------------------

# percentage of all conformity status category for active listings only
january_listings %>% 
  st_drop_geometry() %>% 
  filter(registration_analyzed != "Invalid",
         active >= max(active, na.rm = T) - days(30)) %>% 
  count(registration_analyzed) %>% 
  mutate(per = str_glue("{round(n/sum(n), digits = 3)*100}%"))


## Unsure if relevant
# Graphing the conformity status of displayed listings
#january_listings %>%
#   filter(registration_analyzed != "Invalid") %>%
#   ggplot()+
#   geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
#   xlab("")+
#   ylab("Number of listings")+
#   guides(x = guide_axis(angle = 10))+
#   scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 6, 2, 3, 1)])+
#   theme_minimal()

# Graphing the conformity status of active listings
#january_listings %>%
#   filter(registration_analyzed != "Invalid",
#          active >= max(active, na.rm = T) - days(30)) %>%
#   ggplot()+
#   geom_histogram(stat = "count", aes(registration_analyzed, fill = registration_analyzed))+
#   xlab("")+
#   ylab("Number of listings")+
#   guides(x = guide_axis(angle = 10))+
#   scale_fill_manual(name = "Registration conformity", values = col_palette[c(4, 6, 2, 3, 1)])+
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

# percentage and number of non-conform active listings per area
map_table <- 
  WD %>% 
  st_join(january_listings %>% select(-ward)) %>% 
  filter(active >= max(active, na.rm = T) - days(30), 
         registration_analyzed != "Exempt") %>% 
  count(ward, valid = registration_analyzed == "Conform") %>% 
  group_by(ward) %>% 
  summarize(invalid = n[!valid], invalid_pct = n[!valid] / sum(n))

spadina_illegal_num <- 
  map_table %>% 
  filter(ward == "Spadina-Fort York") %>% 
  pull(invalid) %>%  
  prettyNum(",")

university_illegal_num <- 
  map_table %>% 
  filter(ward == "University-Rosedale") %>% 
  pull(invalid) %>%  
  prettyNum(",")

spadina_illegal_pct <- 
  map_table %>% 
  filter(ward == "Spadina-Fort York") %>% 
  pull(invalid_pct) %>% 
  scales::percent(0.1)

willowdale_illegal_pct <- 
  map_table %>% 
  filter(ward == "Willowdale") %>% 
  pull(invalid_pct) %>% 
  scales::percent(0.1)

scar_n_illegal_pct <- 
  map_table %>% 
  filter(ward == "Scarborough North") %>% 
  pull(invalid_pct) %>% 
  scales::percent(0.1)

map_table %>% 
  ggplot() +
  geom_sf(aes(fill = invalid_pct), colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3, 4, 5)], na.value = "grey80",
                       oob = scales::squish, 
                       labels = scales::percent_format(accuracy = 1))  +
  geom_sf_text(aes(label = invalid), colour = "black") +
  guides(fill = guide_colourbar(title = "Non-conforming listings")) + 
  theme_void() +
  theme(legend.position = "bottom")

########### HERE!! ###########

# Add property information of listings scraped in january
january_listings <- 
  reg_4 %>% 
  select(property_ID, registration, registration_analyzed, postal_code) %>% 
  left_join(., property, by = "property_ID") %>% 
  st_as_sf()

# Percentage of inactive listings 
january_listings %>% 
  st_drop_geometry() %>% 
  group_by(active = active >= max(active, na.rm = T) - days(30)) %>% 
  summarize(percentage_active = n()/nrow(january_listings)) %>% 
  mutate(percentage_active = str_glue("{round(percentage_active, digits = 3)*100}%"))


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

#invalid <- 
#  license_activity %>% 
#  filter(registration_analyzed != "Conform", registration_analyzed != "Exempt") %>% 
#  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
#  mutate(registration_analyzed = "Non-conforming")

all_listings <- 
  license_activity %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(registration_analyzed = "All listings")

license_activity <-
  all_listings %>% 
  bind_rows(license_activity %>% 
              group_by(registration_analyzed) %>%
              summarize(n = n(), across(per_A:revenue_reg, mean))) %>% 
  #bind_rows(invalid) %>% 
  select(registration_analyzed, everything())

invalid <- 
  license_activity %>% 
  filter(registration_analyzed != "Conform", registration_analyzed != "Exempt") %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(registration_analyzed = "Non-conforming")

all_listings <- 
  license_activity %>% 
  summarize(n = n(), across(per_A:revenue_reg, mean)) %>% 
  mutate(registration_analyzed = "All listings")

license_activity <-
  all_listings %>% 
  bind_rows(license_activity %>% 
              group_by(registration_analyzed) %>%
              summarize(n = n(), across(per_A:revenue_reg, mean))) %>% 
  bind_rows(invalid) %>% 
  select(registration_analyzed, everything())

valid_r_pct <- 
  license_activity %>% 
  filter(registration_analyzed == "Conform") %>% 
  pull(per_R) %>% 
  scales::percent(0.1)

no_license_r_pct <- 
  license_activity %>% 
  filter(registration_analyzed == "No license") %>% 
  pull(per_R) %>% 
  scales::percent(0.1)

exempt_r_pct <- 
  license_activity %>% 
  filter(registration_analyzed == "Exempt") %>% 
  pull(per_R) %>% 
  scales::percent(0.1)

exempt_rev <- 
  license_activity %>% 
  filter(registration_analyzed == "Exempt") %>% 
  pull(revenue) %>% 
  scales::dollar(1)

fake_r_pct <- 
  license_activity %>% 
  filter(registration_analyzed == "Fake License") %>% 
  pull(per_R) %>% 
  scales::percent(0.1)

fake_rev <- 
  license_activity %>% 
  filter(registration_analyzed == "Fake License") %>% 
  pull(revenue) %>% 
  scales::dollar(1)

valid_rev <- 
  license_activity %>% 
  filter(registration_analyzed == "Conform") %>% 
  pull(revenue) %>% 
  scales::dollar(1)

license_activity %>% 
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

license_activity[c(1, 7, 6, 8, 5, 4, 3, 2),] %>% 
  select(-n) %>% 
  #mutate(n = prettyNum(n, ",")) %>% 
  mutate(across(per_A:per_B, scales::percent, 0.1)) %>% 
  mutate(across(revenue:revenue_reg, scales::dollar, 1)) %>% 
  set_names(c("License status", "Nights available", 
              "Nights reserved", "Nights blocked", 
              "Revenue per night since Covid-19", 
              "Revenue per night since regulations")) %>% 
  kbl(caption = "STR activity by license status", align = "lrrrrrr") %>%
  kable_styling(latex_options = "scale_down")



### Compliance by commercial type and license conformity ---------------------------------

FREH_10 <-
  daily %>% 
  filter(property_ID %in% january_listings$property_ID) %>% 
  filter(date >= "2020-10-01", FREH_3 >= 0.5) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  mutate(FREH_10 = TRUE) %>% 
  select(property_ID, FREH_10)

FREH_01 <-
  daily %>% 
  filter(property_ID %in% january_listings$property_ID) %>% 
  filter(date >= "2020-01-01", FREH_3 >= 0.5) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  mutate(FREH_01 = TRUE) %>% 
  select(property_ID, FREH_01)

multi_10 <- 
  daily %>% 
  filter(property_ID %in% january_listings$property_ID) %>% 
  filter(date >= "2020-10-01", multi) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  mutate(multi_10 = TRUE) %>% 
  select(property_ID, multi_10)

multi_01 <- 
  daily %>% 
  filter(property_ID %in% january_listings$property_ID) %>% 
  filter(date >= "2020-01-01", multi) %>%
  distinct(property_ID, .keep_all = TRUE) %>% 
  mutate(multi_01 = TRUE) %>% 
  select(property_ID, multi_01)

license_scrape <- 
  january_listings %>% 
  left_join(FREH_10) %>% 
  left_join(FREH_01) %>% 
  left_join(multi_10) %>% 
  left_join(multi_01) %>% 
  mutate(
    status_10 = case_when(
      active < "2020-10-01" ~ "Inactive",
      FREH_10 ~ "FREH",
      multi_10 ~ "Multilisting",
      TRUE ~ "Non-commercial"),
    status_01 = case_when(
      active < "2020-01-01" ~ "Inactive",
      FREH_01 ~ "FREH",
      multi_01 ~ "Multilisting",
      TRUE ~ "Non-commercial")) %>% 
  select(-c(FREH_10:multi_01)) %>% 
  mutate(status_10 = factor(status_10, levels = c("Inactive", "Non-commercial",
                                                  "Multilisting", "FREH")),
         status_01 = factor(status_01, levels = c("Inactive", "Non-commercial",
                                                  "Multilisting", "FREH")),
         license_status = factor(registration_analyzed, 
                                 levels = c("No license", "Fake License", "Invalid", 
                                            "Duplicates", "Exempt", "Conform")))

scraped_number <- 
  nrow(license_scrape) %>% 
  prettyNum(",")

valid_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  summarize(pct = mean(license_status == "Conform")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

exempt_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  summarize(pct = mean(license_status == "Exempt")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

duplicates_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  summarize(pct = mean(license_status == "Duplicates")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)


problem_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  summarize(pct = mean(license_status %in% c("Fake", "Invalid", 
                                             "No license"))) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

freh_valid_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(status_10 == "FREH") %>% 
  summarize(pct = mean(license_status == "Conform")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

freh_no_license_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(status_10 == "FREH") %>% 
  summarize(pct = mean(license_status == "No license")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

ml_duplicate_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(status_10 == "Multilisting") %>% 
  summarize(pct = mean(license_status == "Duplicates")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

min_no_license_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(status_10 != "FREH") %>% 
  summarize(pct = mean(license_status == "No license")) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

eh_multiple_license <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(license_status == "Duplicates", active >= "2020-10-01") %>% 
  filter(listing_type == "Entire home/apt") %>% 
  count(registration, sort = TRUE) %>% 
  filter(n > 1) %>%
  pull(n) %>% 
  sum()

pr_multiple_license <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(license_status == "Duplicates", active >= "2020-10-01") %>% 
  filter(listing_type == "Private room") %>% 
  count(registration, sort = TRUE) %>% 
  filter(n > 1) %>%
  pull(n) %>% 
  sum()

eh_multiple_license_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(license_status == "Duplicates", active >= "2020-10-01") %>% 
  filter(listing_type == "Entire home/apt") %>% 
  nrow() %>% 
  {eh_multiple_license / .} %>% 
  scales::percent(0.1)

pr_multiple_license_pct <- 
  license_scrape %>% 
  st_drop_geometry() %>% 
  filter(license_status == "Duplicates", active >= "2020-10-01") %>% 
  filter(listing_type == "Private room") %>% 
  nrow() %>% 
  {eh_multiple_license / .} %>% 
  scales::percent(0.1)


fig_left_1 <- 
  license_scrape %>%
  ggplot() +
  geom_histogram(aes(reorder(license_status, desc(license_status)), 
                     fill = status_10), stat = "count") + 
  xlab(NULL) +
  scale_fill_manual(name = "Listing status", 
                    values = col_palette[c(6, 2, 1, 4)]) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  theme_minimal() +    
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain"),
        legend.title = element_text(face = "bold",  
                                    size = 10),
        legend.text = element_text(size = 10))

fig_right_1 <- 
  license_scrape %>% 
  ggplot() +
  geom_bar(aes(reorder(license_status, desc(license_status)), 
               fill = status_10), position = "fill", stat = "count") +
  xlab(NULL) +
  scale_fill_manual(name = "Listing status", 
                    values = col_palette[c(6, 2, 1, 4)]) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain"),
        legend.title = element_text(face = "bold",  
                                    size = 10),
        legend.text = element_text(size = 10))

fig_left_2 <- 
  license_scrape %>% 
  ggplot() +
  geom_histogram(aes(reorder(status_10, desc(status_10)), 
                     fill = license_status), stat = "count") + 
  xlab(NULL) +
  scale_fill_manual(name = "License status", 
                    values = col_palette[c(6, 3, 2, 1, 4, 5, 7)]) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain"),
        legend.title = element_text(face = "bold",  
                                    size = 10),
        legend.text = element_text(size = 10))

fig_right_2 <- 
  license_scrape %>% 
  ggplot() +
  geom_bar(aes(reorder(status_10, desc(status_10)), fill = license_status), 
           position = "fill", stat = "count") +
  xlab(NULL) +
  scale_fill_manual(name = "License status", 
                    values = col_palette[c(6, 3, 2, 1, 4, 5, 7)]) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain"),
        legend.title = element_text(face = "bold",  
                                    size = 10),
        legend.text = element_text(size = 10))

p1 <- 
  fig_left_1 + fig_right_1 + plot_layout(guides = 'collect', ncol = 2,
                                         tag_level = 'new') & 
  theme(legend.position = "bottom")

p2 <- 
  fig_left_2 + fig_right_2 + plot_layout(guides = 'collect', ncol = 2,
                                         tag_level = 'new') & 
  theme(legend.position = "bottom")

(p1 / p2) + plot_layout(ncol = 1) + 
  plot_annotation(tag_levels = c("A", "1"))
