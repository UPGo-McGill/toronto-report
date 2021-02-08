source("R/01_startup.R")

###### for max
if(memory.limit()< 20000) {memory.limit(size = 48000)}
####


# Load data ---------------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_processed.qs", nthreads = availableCores())


# Revenue matrice ---------------------------------------------------------

ltr_unique_price <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(price)) %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  select(ltr_ID = id, ltr_price = price, ltr_scraped = scraped, ward, property_ID)

# Monthly revenue in 2019
revenue_2019 <-
  daily %>% 
  filter(date >= start_2019, date <= end_2019,
         status == c("A", "R")) %>% 
  mutate(month = month(date)) %>%
  group_by(property_ID, month) %>% 
  filter(n() >= 5) %>% 
  filter(status == "R") %>% 
  summarize(monthly_revenue_2019 = sum(price)) %>%
  group_by(property_ID) %>%
  summarize(monthly_revenue_2019 = mean(monthly_revenue_2019)) %>% 
  ungroup()

# Monthly revenue in 2020
revenue_2020 <-
  daily %>% 
  filter(date >= start_2020, date <= end_2020,
         status == c("A", "R")) %>% 
  mutate(month = month(date)) %>%
  group_by(property_ID, month) %>% 
  filter(n() >= 5) %>% 
  filter(status == "R") %>% 
  summarize(monthly_revenue_2020 = sum(price)) %>%
  group_by(property_ID) %>%
  summarize(monthly_revenue_2020 = mean(monthly_revenue_2020)) %>% 
  ungroup()

# Monthly revenue variation between 2019 and 2020
monthly_revenue_variation <-
  revenue_2020 %>%
  left_join(revenue_2019) %>%
  mutate(monthly_variation = (monthly_revenue_2020-monthly_revenue_2019)/monthly_revenue_2019)

# Matrice creation --------------------------------------------------------

matrice <-
  monthly_revenue_variation %>%
  inner_join(unnest(select(filter(st_drop_geometry(property), 
                                  !is.na(ab_property), listing_type == "Entire home/apt"), 
                           property_ID, ltr_ID, bedrooms, ward), 
                    ltr_ID),
             by = "property_ID") %>% 
  left_join(select(ltr_unique_price, -property_ID, -ward), by = "ltr_ID") %>% 
  # Keep the most recent LTR price scraped per property_ID
  arrange(desc(ltr_scraped)) %>% 
  distinct(property_ID, .keep_all=T) %>% 
  select(-ltr_scraped) %>% 
  mutate(multi = ifelse(property_ID %in% (daily %>% filter(multi) %>% pull(property_ID) %>% unique()), T, F),
         FREH = ifelse(property_ID %in% (daily %>% filter(FREH_3>0.5) %>% pull(property_ID) %>% unique()), T, F),
         commercial = ifelse(multi== T | FREH == T, T, F))

# Delete outliers
matrice_outly <- 
  matrice %>% 
  filter(ltr_price < as.numeric(quantile(matrice$ltr_price, 0.975, na.rm = T)),
         ltr_price > as.numeric(quantile(matrice$ltr_price, 0.025, na.rm = T)),
         monthly_revenue_2020 < as.numeric(quantile(matrice$monthly_revenue_2020,
                                                    0.975, na.rm = T)),
         monthly_revenue_2020 > as.numeric(quantile(matrice$monthly_revenue_2020,
                                                    0.025, na.rm = T)))


# Fitting the model -------------------------------------------------------

#' What would be the amount a landlord would accept to put their unit on the ltr
#' market? Linked to financial performance, size and location.

library(caret)

# Preparing a location variable
ward_price_bounds <- 
  ltr_unique_price %>% 
  filter(ltr_price < as.numeric(quantile(ltr_unique_price$ltr_price, 0.99)),
         ltr_price > as.numeric(quantile(ltr_unique_price$ltr_price, 0.025))) %>% 
  group_by(ward) %>% 
  summarize(ltr_mean_price = mean(ltr_price), n()) %>% 
  mutate(geo_quant = percent_rank(ltr_mean_price)) %>%
  mutate(geo_quant = case_when(geo_quant <= 0.75 ~ "lower",
                               # geo_quant <= 0.66 ~ "middle",
                               TRUE ~ "higher"),
         geo_quant = factor(geo_quant, levels = c("lower",# "middle", 
                                                  "higher"))) %>%
  select(ward, geo_quant)

# Final dataframe to fit the model
matrice_model <- 
  matrice_outly %>% 
  left_join(ward_price_bounds, by = "ward") %>%
  select(-c("monthly_variation", "monthly_revenue_2019",  
            "property_ID", "ltr_ID", "ward", "FREH", "multi", "commercial", 
            "ward"))

# Fit the final model
model <-
  lm(ltr_price ~ ., data = matrice_model)

summary(model)


# Apply the model to the population of STR --------------------------------

listings_pred <- 
  matrice %>% 
  left_join(ward_price_bounds, by = "ward") %>% 
  modelr::add_predictions(model, type = "response") %>% 
  select(property_ID, monthly_revenue_2020, bedrooms, ward, pred)

# Is the listing still listed?
listings_pred <- 
listings_pred %>% 
  left_join(select(st_drop_geometry(property), property_ID, scraped, active), by = "property_ID") %>% 
  mutate(still_listed = if_else(scraped >= max(scraped) - days(30), TRUE, FALSE),
         still_active = if_else(active >= max(active, na.rm = T) - days(30), TRUE, FALSE)) %>% 
  select(-scraped, -active)

# Adding registration number, if any

reg <- qread("data/reg_4.qs", nthreads = availableCores())

listings_pred <- 
listings_pred %>% 
  left_join(select(reg, -date), by = "property_ID")

# Adding street address, if matched in the LTR
location <- 
listings_pred %>% 
  left_join(filter(select(unnest(st_drop_geometry(ltr), property_ID),property_ID, location), !is.na(location)), by = "property_ID") %>% 
  distinct() %>% 
  group_by(property_ID) %>% 
  summarize(location = list(location))

listings_pred <- 
listings_pred %>% 
  left_join(location) %>%
  mutate(location = map_chr(location, paste0, collapse = "; ")) 

# Airbnb URL
listings_pred <-
  listings_pred %>% 
  mutate(url = paste0("https://airbnb.ca/rooms/", str_remove(property_ID, 
                                                             "ab-")))

# Rename every columns
listings_pred <- 
  listings_pred %>% 
  transmute(`Property ID` = property_ID,
            `Still listed?` = still_listed,
            `Still active?` = still_active,
            `2020 Monthly Revenue` = monthly_revenue_2020,
            `Number of bedrooms` = bedrooms,
            Ward = ward,
            `Registration number` = registration,
            `Street address from KJ or CL` = location,
            `Predicted asking rent` = pred,
            URL = url)


# Save the CSV containing predictions -------------------------------------


write_csv(listings_pred, "output/listings_pred.csv")

qsavem(model, file = "output/listings_pred_model.qs")