#### 11 REGISTRATION SCRAPE ####################################################

#' This script should be rerun when new STR listings are added to the dataset.
#' 
#' Output:
#' - `str_raw.qsm` (updated)
#' 
#' Script dependencies:
#' - `03_str_data_import.R`
#' 
#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())


# Get existing registration scrapes from server ---------------------------

upgo_connect(registration = TRUE)

registration_old <- 
  registration_remote %>% 
  filter(property_ID %in% !!property$property_ID) %>% 
  collect() %>% 
  group_by(property_ID) %>% 
  filter(date == max(date)) %>% 
  ungroup()

registration <- registration_old

if (!is.null(property$registration)) property$registration <- NULL


# Scrape new properties ---------------------------------------------------

upgo_scrape_connect()

n <- 1

while (nrow(filter(property, !property_ID %in% registration$property_ID)) > 0 && 
       n < 20) {
  
  n <- n + 1
  
  new_scrape <- 
    property %>%
    st_drop_geometry() %>% 
    filter(!property_ID %in% registration$property_ID) %>% 
    dplyr::slice(1:2000) %>% 
    upgo_scrape_ab_registration(proxies = .proxy_list, cores = 10)
  
  registration <- 
    registration %>% 
    bind_rows(new_scrape)
  
  qsave(registration, file = "output/registration.qs",
        nthreads = availableCores())  
  
}


# Recheck NA scrapes ------------------------------------------------------

NA_scrapes <- 
  registration %>% 
  filter(is.na(registration))

NA_scrapes_checked <- NA_scrapes[0,]

while (nrow(filter(NA_scrapes, 
                   !property_ID %in% NA_scrapes_checked$property_ID)) > 0 && 
       n < 20) {
  
  n <- n + 1
  
  new_scrape <- 
    NA_scrapes %>%
    filter(!property_ID %in% NA_scrapes_checked$property_ID) %>% 
    dplyr::slice(1:2000) %>% 
    upgo_scrape_ab_registration(proxies = .proxy_list, cores = 10)
  
  NA_scrapes_checked <- 
    NA_scrapes_checked %>% 
    bind_rows(new_scrape)
  
}


# Add new scrapes to server -----------------------------------------------

registration_new <- 
  registration %>% 
  anti_join(registration_old)

NA_scrapes_new <- 
  NA_scrapes_checked %>% 
  anti_join(registration, by = c("property_ID", "date"))

RPostgres::dbWriteTable(.con, "registration", registration_new, append = TRUE)
RPostgres::dbWriteTable(.con, "registration", NA_scrapes_new, append = TRUE)

upgo_scrape_disconnect()


# Consolidate output ------------------------------------------------------

registration_table <- 
  registration %>% 
  bind_rows(NA_scrapes_new) %>% 
  arrange(property_ID, date) %>% 
  group_by(property_ID) %>% 
  filter(date == max(date)) %>% 
  ungroup()

rm(registration)


# Clean output ------------------------------------------------------------

registration_table <-
  registration_table %>%
  mutate(registration = case_when(
    is.na(registration) ~ NA_character_,
    registration == "NO LISTING" ~ "NO LISTING",
    registration == "HOMEAWAY" ~ "HOMEAWAY",
    str_detect(registration, '(E|e)xempt') ~ "EXEMPT",
    {registration %>% 
        str_remove_all("\\D") %>% 
        nchar()} == 8L ~ str_remove_all(registration, "\\D"),
    TRUE ~ "INVALID"
  )) %>% 
  mutate(registration = if_else(str_starts(registration, "\\D|18|19|20"),
                                registration, "INVALID")) %>% 
  mutate(registration = if_else(str_detect(registration, "[:digit:]"),
                                paste0(substr(registration, 1, 2), "-",
                                       substr(registration, 3, 8)),
                                registration))


# Add results to property table -------------------------------------------

property <- 
  registration_table %>% 
  select(-date) %>% 
  left_join(property, .)


# Save output -------------------------------------------------------------

qsavem(property, daily, host, exchange_rates, file = "output/str_raw.qsm",
       nthreads = availableCores())
