source("R/01_startup.R")


# Load data ---------------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_raw.qs", nthreads = availableCores())


# Revenue matrice ---------------------------------------------------------

ltr_unique_price <- 
  ltr %>% 
  # keep the more recent price
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  select(ltr_ID = id, ltr_price = price)

# Monthly revenue in 2019
revenue_2019 <-
daily %>% 
  filter(date >= "2019-01-01", date <= "2019-12-31",
         status == c("A", "R")) %>% 
  group_by(property_ID) %>% 
  filter(n() >= (365/12)) %>% 
  mutate(max_date_2019 = max(date),
         min_date_2019 = min(date)) %>% 
  filter(status == "R") %>% 
  summarize(monthly_revenue_2019 = sum(price)/((as.numeric(max_date_2019-min_date_2019)/(365/12)))) %>%
  distinct() %>%
  arrange(-monthly_revenue_2019) %>% 
  ungroup()

# Monthly revenue in 2020
revenue_2020 <-
  daily %>% 
  filter(date >= "2020-01-01", date <= "2020-12-31",
         status == c("A", "R")) %>% 
  group_by(property_ID) %>% 
  filter(n() >= (365/12)) %>% 
  mutate(max_date_2020 = max(date),
         min_date_2020 = min(date)) %>% 
  filter(status == "R") %>% 
  summarize(monthly_revenue_2020 = sum(price)/((as.numeric(max_date_2020-min_date_2020)/(365/12)))) %>%
  distinct() %>%
  arrange(-monthly_revenue_2020) %>% 
  ungroup()

# Monthly revenue variation between 2019 and 2020
monthly_revenue_variation <- 
revenue_2020 %>% 
  # I left_join 2019 to 2020 because we do not care about properties only present
  # in 2019
  left_join(revenue_2019) %>% 
  mutate(monthly_variation = (monthly_revenue_2020-monthly_revenue_2019)/monthly_revenue_2019)

# Property status (still on the platform or not?)
property_status <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-03-01") %>% 
  mutate(status = case_when(active <= max(scraped)-months(1) ~ "Blocked",
                            scraped <= max(scraped)-months(1) ~ "Taken down",
                            TRUE ~ "Continued")) %>% 
  select(property_ID, moved)

# Matrice creation
matrice <-
  monthly_revenue_variation %>% 
  left_join(select(st_drop_geometry(property), property_ID, ltr_ID), by = "property_ID") %>% 
  left_join(ltr_unique_price, by = "ltr_ID") %>% 
  left_join(property_status, by = "property_ID")



# Analysis ----------------------------------------------------------------

#' Grouped by status (taken down, blocked, continued operation): analyse the asking
#' rent vs financial performance. Is there a pattern? Maybe the previous financial
#' performance predicts moving or not, or maybe it is the asking rent that predicts
#' it.
#' 
#' Look if there is a pattern, a relationship between financian performance and
#' asking rent. What could the city do rentwise for landlors to accept to rent 
#' long term? How much a month would they go for.
#' 
#' Is there a pattern with geography? Maybe try and control this parameter for
#' the analysis to work best.


# Fitting the model -------------------------------------------------------

#' What would be the amount a landlord would accept to put their unit on the ltr
#' market? Is it linked to precedent financial performance?
#' 
#' What type of model is needed? Binomial family because we have data on listings
#' that succeeded and the ones that didn't, or a normal multivariate linear regression
#' model only looking at the listings that succesfully moved?

model <-
  lm(price ~ ., data = (matrice %>% 
                          filter(status == c("Blocked", "Taken down")) %>% 
                          select(-status, -property_ID, -ltr_ID)))

summary(model)

