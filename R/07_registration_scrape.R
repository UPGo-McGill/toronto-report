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
reg_table <- qread("data/reg.qs", nthreads = availableCores())


# # Get existing registration scrapes from server ---------------------------
# 
# upgo_connect(registration = TRUE)
# 
# registration_old <- 
#   registration_remote %>% 
#   filter(property_ID %in% !!property$property_ID) %>% 
#   collect() %>% 
#   group_by(property_ID) %>% 
#   filter(date == max(date)) %>% 
#   ungroup()
# 
# registration <- registration_old
# 
# if (!is.null(property$registration)) property$registration <- NULL
# 
# 
# # Scrape new properties ---------------------------------------------------
# 
# upgo_scrape_connect()
# 
# n <- 1
# 
# while (nrow(filter(property, !property_ID %in% registration$property_ID)) > 0 && 
#        n < 20) {
#   
#   n <- n + 1
#   
#   new_scrape <- 
#     property %>%
#     st_drop_geometry() %>% 
#     filter(!property_ID %in% registration$property_ID) %>% 
#     dplyr::slice(1:2000) %>% 
#     upgo_scrape_ab_registration(proxies = .proxy_list, cores = 10)
#   
#   registration <- 
#     registration %>% 
#     bind_rows(new_scrape)
#   
#   qsave(registration, file = "output/registration.qs",
#         nthreads = availableCores())  
#   
# }
# 
# 
# # Recheck NA scrapes ------------------------------------------------------
# 
# NA_scrapes <- 
#   registration %>% 
#   filter(is.na(registration))
# 
# NA_scrapes_checked <- NA_scrapes[0,]
# 
# while (nrow(filter(NA_scrapes, 
#                    !property_ID %in% NA_scrapes_checked$property_ID)) > 0 && 
#        n < 20) {
#   
#   n <- n + 1
#   
#   new_scrape <- 
#     NA_scrapes %>%
#     filter(!property_ID %in% NA_scrapes_checked$property_ID) %>% 
#     dplyr::slice(1:2000) %>% 
#     upgo_scrape_ab_registration(proxies = .proxy_list, cores = 10)
#   
#   NA_scrapes_checked <- 
#     NA_scrapes_checked %>% 
#     bind_rows(new_scrape)
#   
# }
# 
# 
# # Add new scrapes to server -----------------------------------------------
# 
# registration_new <- 
#   registration %>% 
#   anti_join(registration_old)
# 
# NA_scrapes_new <- 
#   NA_scrapes_checked %>% 
#   anti_join(registration, by = c("property_ID", "date"))
# 
# RPostgres::dbWriteTable(.con, "registration", registration_new, append = TRUE)
# RPostgres::dbWriteTable(.con, "registration", NA_scrapes_new, append = TRUE)
# 
# upgo_scrape_disconnect()
# 
# 
# # Consolidate output ------------------------------------------------------
# 
# registration_table <- 
#   registration %>% 
#   bind_rows(NA_scrapes_new) %>% 
#   arrange(property_ID, date) %>% 
#   group_by(property_ID) %>% 
#   filter(date == max(date)) %>% 
#   ungroup()
# 
# rm(registration)
# 
# 
# # Clean output ------------------------------------------------------------

registration_table <-
  registration_table %>%
  mutate(registration - toupper(registration),
         registration = case_when(
    is.na(registration) ~ NA_character_,
    registration == "NO LISTING" ~ "NO LISTING",
    registration == "HOMEAWAY" ~ "HOMEAWAY",
    registration == "EXEMPT" ~ "EXEMPT",
    str_detect(registration, "STR-\\d{4}-\\w{6}") ~ registration,
    TRUE ~ "INVALID"
  ))


# # Add results to property table -------------------------------------------

property <- 
  property %>% 
  select(-registration_scraped_date, -registration) %>% 
  left_join(rename(reg_table, registration_scraped_date = date)) %>% 
  mutate(registration = ifelse(!property_ID %in% reg_table$property_ID, "NOT SCRAPED", registration))



# Save output -------------------------------------------------------------

qsavem(property, daily, #host, exchange_rates, 
       file = "output/str_raw.qsm",
       nthreads = availableCores())
