#### 16 MONTREAL-TORONTO COMPARISON ############################################

#' This script is slow to run; it should be rerun whenever STR data changes.

#' Output:
#' - `cma_comparison.qsm` (updated)
#' 
#' Script dependencies:
#' - `05_str_cma_data_import.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(caret)


# Load data ---------------------------------------------------------------

qload("output/cma_comparison.qsm", nthreads = availableCores())



# Remove non-city ---------------------------------------------------------

property_montreal_other <- 
  property_montreal %>% 
  filter(!central)

property_montreal <- 
  property_montreal %>% 
  filter(central)

property_toronto_other <- 
  property_toronto %>% 
  filter(!central)

property_toronto <- 
  property_toronto %>% 
  filter(central)

daily_montreal_other <- 
  daily_montreal %>% 
  filter(!central)

daily_montreal <- 
  daily_montreal %>% 
  filter(central)

daily_toronto_other <- 
  daily_toronto %>% 
  filter(!central)

daily_toronto <- 
  daily_toronto %>% 
  filter(central)


# Calculate FREH ----------------------------------------------------------

# Prepare daily files -----------------------------------------------------

daily_montreal <- 
  daily_montreal %>% 
  mutate(year = year(date),
         month = month(date))

daily_montreal_other <- 
  daily_montreal_other %>% 
  mutate(year = year(date),
         month = month(date))

daily_toronto <- 
  daily_toronto %>% 
  mutate(year = year(date),
         month = month(date))

daily_toronto_other <- 
  daily_toronto_other %>% 
  mutate(year = year(date),
         month = month(date))


# Get traditional FREH status ---------------------------------------------

FREH_montreal <- 
  daily_montreal %>% 
  strr_FREH() %>% 
  filter(FREH)

FREH_montreal_other <- 
  daily_montreal_other %>% 
  strr_FREH() %>% 
  filter(FREH)

FREH_toronto <- 
  daily_toronto %>% 
  strr_FREH() %>% 
  filter(FREH)

FREH_toronto_other <- 
  daily_toronto_other %>% 
  strr_FREH() %>% 
  filter(FREH)


# Produce monthly activity table for all EH listings ----------------------

monthly_montreal <-
  daily_montreal %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH_montreal, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property_montreal, property_ID, created),
            by = "property_ID") %>% 
  # Trim listings to the start of the month--can later test if the extra
  # complexity produces meaningful model improvement
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month)) %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")

monthly_montreal_other <-
  daily_montreal_other %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH_montreal_other, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property_montreal_other, property_ID, created),
            by = "property_ID") %>% 
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month)) %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")

monthly_toronto <-
  daily_toronto %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH_toronto, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property_toronto, property_ID, created),
            by = "property_ID") %>% 
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month)) %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")

monthly_toronto_other <-
  daily_toronto_other %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH_toronto_other, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property_toronto_other, property_ID, created),
            by = "property_ID") %>% 
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month)) %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")


# Get listings active for a year or more, and take the first year ---------

first_year_montreal <-
  monthly_montreal %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  group_by(property_ID) %>% 
  filter(n() >= 12) %>% 
  ungroup() %>% 
  filter(month_since_created <= 11) %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A)

first_year_montreal_other <-
  monthly_montreal_other %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  group_by(property_ID) %>% 
  filter(n() >= 12) %>% 
  ungroup() %>% 
  filter(month_since_created <= 11) %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A)

first_year_toronto <-
  monthly_toronto %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  group_by(property_ID) %>% 
  filter(n() >= 12) %>% 
  ungroup() %>% 
  filter(month_since_created <= 11) %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A)

first_year_toronto_other <-
  monthly_toronto_other %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  group_by(property_ID) %>% 
  filter(n() >= 12) %>% 
  ungroup() %>% 
  filter(month_since_created <= 11) %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A)


# Fit model and apply to listings < 1 year old ----------------------------

model_12_montreal <- glm(FREH ~ cum_R + cum_AR + month_since_created + month, 
                         data = first_year_montreal, family = binomial)

model_12_toronto <- glm(FREH ~ cum_R + cum_AR + month_since_created + month,
                        data = first_year_toronto, family = binomial)

model_12_results_montreal <- 
  monthly_montreal %>% 
  group_by(property_ID) %>% 
  filter(max(month_since_created) < 12) %>% 
  ungroup() %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A) %>% 
  modelr::add_predictions(model_12_montreal, type = "response") %>% 
  mutate(FREH = if_else(FREH, as.numeric(FREH), pred)) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_12_results_toronto <- 
  monthly_toronto %>% 
  group_by(property_ID) %>% 
  filter(max(month_since_created) < 12) %>% 
  ungroup() %>% 
  group_by(property_ID) %>% 
  mutate(FREH = as.logical(ceiling(mean(FREH))),
         month = month.name[.data$month],
         cum_R = cumsum(R),
         cum_A = cumsum(A),
         cum_AR = cum_A + cum_R) %>% 
  ungroup() %>% 
  select(-cum_A) %>% 
  modelr::add_predictions(model_12_toronto, type = "response") %>% 
  mutate(FREH = if_else(FREH, as.numeric(FREH), pred)) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

daily_montreal <-
  daily_montreal %>% 
  left_join(select(monthly_montreal, property_ID, year, month, FREH),
            by = c("property_ID", "year", "month")) %>% 
  left_join(select(model_12_results_montreal, property_ID:month, prob = FREH),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH = case_when(
    !is.na(prob) ~ prob,
    !is.na(FREH) ~ as.numeric(FREH),
    TRUE ~ 0)) %>% 
  select(-prob)

daily_toronto <-
  daily_toronto %>% 
  left_join(select(monthly_toronto, property_ID, year, month, FREH),
            by = c("property_ID", "year", "month")) %>% 
  left_join(select(model_12_results_toronto, property_ID:month, prob = FREH),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH = case_when(
    !is.na(prob) ~ prob,
    !is.na(FREH) ~ as.numeric(FREH),
    TRUE ~ 0)) %>% 
  select(-prob)


# Model based on last 3 months --------------------------------------------

# Summarize by month
after_one_year_montreal <- 
  monthly_montreal %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)

after_one_year_montreal_other <- 
  monthly_montreal_other %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)

after_one_year_toronto <- 
  monthly_toronto %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)

after_one_year_toronto_other <- 
  monthly_toronto_other %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)


# Fit models and apply to listings > 2 months -----------------------------

model_3_montreal <- glm(FREH ~ R_3 + AR_3 + month, 
                        data = after_one_year_montreal, family = binomial)

model_3_toronto <- glm(FREH ~ R_3 + AR_3 + month, 
                       data = after_one_year_toronto, family = binomial)

model_3_results_montreal <- 
  monthly_montreal %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3_montreal, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_3_results_montreal_other <- 
  monthly_montreal_other %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3_montreal, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_3_results_toronto <- 
  monthly_toronto %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3_toronto, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_3_results_toronto_other <- 
  monthly_toronto_other %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3_toronto, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

daily_montreal <-
  daily_montreal %>% 
  left_join(select(model_3_results_montreal, property_ID, year, month, FREH_3),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily_montreal <- daily_montreal %>% select(-year, -month)

daily_montreal_other <- 
  daily_montreal_other %>% 
  left_join(select(model_3_results_montreal_other, property_ID, year, month, 
                   FREH_3), by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily_montreal_other <- daily_montreal_other %>% select(-year, -month)

daily_toronto <-
  daily_toronto %>% 
  left_join(select(model_3_results_toronto, property_ID, year, month, FREH_3),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily_toronto <- daily_toronto %>% select(-year, -month)

daily_toronto_other <-
  daily_toronto_other %>% 
  left_join(select(model_3_results_toronto_other, property_ID, year, month, 
                   FREH_3), by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily_toronto_other <- daily_toronto_other %>% select(-year, -month)


# Save output -------------------------------------------------------------

qsavem(property_montreal, property_toronto, daily_montreal, daily_toronto,
       property_montreal_other, property_toronto_other, 
       daily_montreal_other, daily_toronto_other,
       file = "output/cma_comparison.qsm", nthreads = availableCores())
