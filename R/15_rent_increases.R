#### 15 RENT INCREASES #########################################################

#' This script is fast to run; it should be rerun whenever STR data changes.

#' Output:
#' - `rent_increases.qsm`
#' 
#' Script dependencies:
#' - `06_cmhc_data_import.R`
#' - `12_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load data ---------------------------------------------------------------

qload("output/cmhc.qsm", nthreads = availableCores())
qload("output/str_processed.qsm", nthreads = availableCores())

# The magic value derived from Barron et al.
magic_value <- 0.00651547619


# Table for entire city ---------------------------------------------------

# Calculate rent increase as magic value * % total listings created in a year
rent_increase <-
  property %>% 
  st_drop_geometry() %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2014", "old", 
                                year_created)) %>%
  group_by(year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(
    rent_increase = slide_dbl(n, ~{
      magic_value * .x[length(.x)] / sum(.x[-length(.x)])}, .before = n() - 1),
    rent_increase = if_else(is.infinite(rent_increase), NA_real_, rent_increase)
    )


# Table for CMHC zones ----------------------------------------------------

rent_increase_zone <-
  property %>% 
  st_intersection(cmhc) %>% 
  st_drop_geometry() %>% 
  group_by(zone) %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2014", "old", 
                                year_created)) %>% 
  group_by(zone, year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(
    rent_increase = slide_dbl(n, ~{
      magic_value * .x[length(.x)] / sum(.x[-length(.x)])}, .before = n() - 1),
    rent_increase = if_else(is.infinite(rent_increase), NA_real_, 
                            rent_increase)) %>% 
  ungroup()


# Save output -------------------------------------------------------------

qsavem(rent_increase, rent_increase_zone, file = "output/rent_increases.qsm",
       nthreads = availableCores())
