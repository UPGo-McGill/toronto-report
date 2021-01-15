#### 13 FREH MODEL #############################################################

#' This script is moderately time-consuming to run; it should be rerun whenever 
#' STR data changes.
#' 
#' Output:
#' - `str_processed.qsm` (updated)
#' - `str_bc_processed.qsm` (updated)
#' - `FREH_model.qsm`
#' 
#' Script dependencies:
#' - `12_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(caret)


# Load data ---------------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/str_bc_processed.qsm", nthreads = availableCores())


# Prepare daily files -----------------------------------------------------

daily <- 
  daily %>% 
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6, 7)))

daily_bc <- 
  daily_bc %>% 
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6, 7)))


# Get traditional FREH status ---------------------------------------------

FREH <- 
  daily %>% 
  strr_FREH() %>% 
  filter(FREH)

FREH_bc <- 
  daily_bc %>% 
  strr_FREH() %>% 
  filter(FREH)


# Produce monthly activity table for all EH listings ----------------------

monthly <-
  daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property, property_ID, created),
            by = "property_ID") %>% 
  # Trim listings to the start of the month--can later test if the extra
  # complexity produces meaningful model improvement
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = as.numeric(substr(created, 1, 4)),
         created_month = as.numeric(substr(created, 6, 7)),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month)) %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")

monthly_bc <-
  daily_bc %>% 
  filter(listing_type == "Entire home/apt") %>% 
  left_join(FREH, by = c("property_ID", "date")) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH)) %>% 
  left_join(select(property_bc, property_ID, created),
            by = "property_ID") %>% 
  # Trim listings to the start of the month--can later test if the extra
  # complexity produces meaningful model improvement
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = as.numeric(substr(created, 1, 4)),
         created_month = as.numeric(substr(created, 6, 7)),
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

first_year <-
  monthly %>% 
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

first_year_bc <-
  monthly_bc %>% 
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

model_12 <- glm(FREH ~ cum_R + cum_AR + month_since_created + month, 
             data = first_year, family = binomial)

model_12_results <- 
  monthly %>% 
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
  modelr::add_predictions(model_12, type = "response") %>% 
  mutate(FREH = if_else(FREH, as.numeric(FREH), pred)) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_12_results_bc <- 
  monthly_bc %>% 
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
  modelr::add_predictions(model_12, type = "response") %>% 
  mutate(FREH = if_else(FREH, as.numeric(FREH), pred)) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

daily <-
  daily %>% 
  left_join(select(monthly, property_ID, year, month, FREH),
            by = c("property_ID", "year", "month")) %>% 
  left_join(select(model_12_results, property_ID:month, prob = FREH),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH = case_when(
    !is.na(prob) ~ prob,
    !is.na(FREH) ~ as.numeric(FREH),
    TRUE ~ 0)) %>% 
  select(-prob)
  
daily_bc <-
  daily_bc %>% 
  left_join(select(monthly_bc, property_ID, year, month, FREH),
            by = c("property_ID", "year", "month")) %>% 
  left_join(select(model_12_results_bc, property_ID:month, prob = FREH),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH = case_when(
    !is.na(prob) ~ prob,
    !is.na(FREH) ~ as.numeric(FREH),
    TRUE ~ 0)) %>% 
  select(-prob)


# # Model testing -----------------------------------------------------------
# 
# # Split the data into training and test set
# training_samples_12 <-
#   first_year$FREH %>%
#   createDataPartition(p = 0.80, list = FALSE)
# 
# train_data_12 <- first_year[training_samples_12, ]
# test_data_12 <- first_year[-training_samples_12, ]
# 
# # Fit the model
# model_12_test <- glm(FREH ~ cum_R + cum_AR + month_since_created + month,
#                   data = train_data_12, family = binomial)
# 
# # Test model
# probabilities_12 <- model_12_test %>% predict(test_data_12, type = "response")
# predicted_classes_12 <- ifelse(probabilities_12 > 0.5, "TRUE", "FALSE")
# mean(predicted_classes_12 == test_data_12$FREH)
# # Outcome: 0.866


# Model based on last 3 months --------------------------------------------

# Summarize by month
after_one_year <- 
  monthly %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)

after_one_year_bc <- 
  monthly_bc %>% 
  filter(year <= 2017 | (year == 2018 & month <= 7)) %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)


# Fit models and apply to listings > 2 months -----------------------------

model_3 <- glm(FREH ~ R_3 + AR_3 + month, data = after_one_year, 
               family = binomial)

model_3_results <- 
  monthly %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

model_3_results_bc <- 
  monthly_bc %>% 
  mutate(month = month.name[.data$month],
         AR = A + R) %>% 
  group_by(property_ID) %>% 
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(R_3), !is.na(AR_3)) %>% 
  modelr::add_predictions(model_3, type = "response") %>% 
  mutate(FREH_3 = pred) %>% 
  select(-pred) %>% 
  rowwise() %>% 
  mutate(month = which(month.name == month)) %>% 
  ungroup()

daily <-
  daily %>% 
  left_join(select(model_3_results, property_ID, year, month, FREH_3),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily <- daily %>% select(-year, -month)

daily_bc <-
  daily_bc %>% 
  left_join(select(model_3_results_bc, property_ID, year, month, FREH_3),
            by = c("property_ID", "year", "month")) %>% 
  mutate(FREH_3 = if_else(is.na(FREH_3), 0, FREH_3))

daily_bc <- daily_bc %>% select(-year, -month)


# # Model testing -----------------------------------------------------------
# 
# # Split the data into training and test set
# training_samples_3 <-
#   after_one_year$FREH %>% createDataPartition(p = 0.80, list = FALSE)
# 
# train_data_3 <- after_one_year[training_samples_3, ]
# test_data_3 <- after_one_year[-training_samples_3, ]
# 
# # Fit the model
# model_3_test <- glm(FREH ~ R_3 + AR_3 + month, data = train_data_3,
#                           family = binomial)
# 
# # Test models
# probabilities_3 <- model_3_test %>% predict(test_data_3, type = "response")
# predicted_classes_3 <- ifelse(probabilities_3 > 0.5, "TRUE", "FALSE")
# mean(predicted_classes_3 == test_data_3$FREH)
# # Outcome: 0.844


# Save output -------------------------------------------------------------

qsavem(property, daily, GH, file = "output/str_processed.qsm",
       nthreads = availableCores())

qsavem(property_bc, daily_bc, file = "output/str_bc_processed.qsm",
       nthreads = availableCores())

qsavem(FREH, FREH_bc, monthly, monthly_bc, first_year, first_year_bc, model_12, 
       model_12_results, model_12_results_bc, after_one_year, after_one_year_bc,
       model_3, model_3_results, model_3_results_bc, 
       file = "output/FREH_model.qsm", nthreads = availableCores())
