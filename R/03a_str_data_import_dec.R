#### 03B STR DATA IMPORT SUPPLEMENT ############################################

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Import December files ---------------------------------------------------

prop_dec <- read_csv("data/prop_dec.csv",
                     col_types = cols_only(
                       `Property ID` = col_character(),
                       `Listing Title` = col_character(),
                       `Property Type` = col_character(),
                       `Listing Type` = col_character(),
                       `Created Date` = col_date(format = ""),
                       `Last Scraped Date` = col_date(format = ""),
                       Country = col_character(),
                       Latitude = col_double(),
                       Longitude = col_double(),
                       State = col_character(),
                       City = col_character(),
                       Neighborhood = col_character(),
                       `Metropolitan Statistical Area` = col_character(),
                       `Currency Native` = col_character(),
                       Bedrooms = col_double(),
                       Bathrooms = col_double(),
                       `Max Guests` = col_double(),
                       `Response Rate` = col_double(),
                       `Airbnb Superhost` = col_logical(),
                       `HomeAway Premier Partner` = col_logical(),
                       `Cancellation Policy` = col_character(),
                       `Security Deposit (USD)` = col_double(),
                       `Cleaning Fee (USD)` = col_double(),
                       `Extra People Fee (USD)` = col_double(),
                       `Check-in Time` = col_character(),
                       `Checkout Time` = col_character(),
                       `Minimum Stay` = col_double(),
                       `Number of Reviews` = col_double(),
                       `Number of Photos` = col_double(),
                       `Instantbook Enabled` = col_logical(),
                       `Overall Rating` = col_double(),
                       `Airbnb Property ID` = col_character(),
                       `Airbnb Host ID` = col_character(),
                       `Airbnb Listing Main Image URL` = col_character(),
                       `HomeAway Property ID` = col_character(),
                       `HomeAway Property Manager` = col_character(),
                       `HomeAway Listing Main Image URL` = col_character()))

daily_dec <-
  read_csv("data/daily_dec.csv", col_types = "cDcDddcddc") %>%
  select(`Property ID`, Date, Status, `Booked Date`, `Price (USD)`,
         `Reservation ID`)

output <-
  prop_dec %>%
  strr_process_property()

prop_dec <- output[[1]]

upgo_connect()

prop_dec <-
  property_remote %>%
  select(property_ID, first_active, last_active) %>%
  left_join(prop_dec, ., copy = TRUE) %>%
  relocate(first_active, last_active, .after = scraped)

prop_dec <-
  daily_remote %>%
  group_by(property_ID) %>%
  filter(property_ID %in% !! filter(prop_dec, is.na(created))$property_ID,
         start_date == min(start_date[status != "U"], na.rm = TRUE)) %>%
  collect() %>%
  select(property_ID, created2 = start_date) %>%
  left_join(prop_dec, ., by = "property_ID") %>%
  mutate(created = if_else(is.na(created), created2, created)) %>%
  select(-created2)

prop_dec <-
  daily_remote %>%
  group_by(property_ID) %>%
  filter(property_ID %in% !! filter(prop_dec, is.na(scraped))$property_ID,
         end_date == max(end_date[status != "U"], na.rm = TRUE)) %>%
  collect() %>%
  select(property_ID, scraped2 = end_date) %>%
  left_join(prop_dec, ., by = "property_ID") %>%
  mutate(scraped = if_else(is.na(scraped), scraped2, scraped)) %>%
  select(-scraped2) %>%
  filter(!is.na(scraped))

upgo_disconnect()

prop_dec <-
  daily_dec %>%
  set_names(c("property_ID", "date", "status", "booked_date", "price", "res_ID")) %>%
  filter(status %in% c("A", "R")) %>%
  group_by(property_ID) %>%
  summarize(first = min(date),
            last = max(date)) %>%
  left_join(prop_dec, ., by = "property_ID") %>%
  mutate(first_active = pmin(first_active, first, na.rm = TRUE),
         last_active = pmax(last_active, last, na.rm = TRUE)) %>%
  select(-first, -last)

daily_created <-
  daily_dec %>%
  set_names(c("property_ID", "date", "status", "booked_date", "price", "res_ID")) %>%
  filter(status != "U") %>%
  group_by(property_ID) %>%
  summarize(created_new = min(date))

prop_dec <-
  prop_dec %>%
  left_join(daily_created, by = "property_ID") %>%
  mutate(created = if_else(is.na(created), created_new, created)) %>%
  select(-created_new)

output <-
  daily_dec %>%
  strr_process_daily(prop_dec)

daily_dec <- output[[1]]
daily_inactive_dec <- output[[2]]

qsavem(prop_dec, daily_dec, daily_inactive_dec, file = "output/str_dec.qsm",
       nthreads = availableCores())

