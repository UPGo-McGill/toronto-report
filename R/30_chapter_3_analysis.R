#### 30 CHAPTER 3 ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 3. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `09_str_processing.R`
#' - `12_rent_increases.R`
#' - `13_condo_analysis.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

qload("output/str_processed.qs", nthreads = availableCores())
load("output/geometry.Rdata")
qload("output/cmhc.qs", nthreads = availableCores())
qload("output/rent_increases.qsm", nthreads = availableCores())
# load("output/condo_analysis.Rdata")


# Prepare new objects -----------------------------------------------------

FREH <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

FREH_area <- 
  daily %>% 
  filter(date %in% as.Date(c("2018-12-01", "2019-12-01"))) %>% 
  group_by(area, date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  mutate(date = if_else(date >= LTM_start_date, 2019, 2018))

GH_area <- 
  GH %>% 
  filter(date %in% as.Date(c(LTM_end_date - years(1), LTM_end_date))) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(LA) %>% 
  st_drop_geometry() %>% 
  group_by(area, date) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE)) %>% 
  as_tibble() %>% 
  mutate(date = if_else(date >= LTM_start_date, 2019, 2018))

FREH_total <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH = sum(housing_units)) %>%
  mutate(GH = slide_dbl(GH, mean, .before = 29))

housing_loss <-
  FREH_total %>%
  select(date, FREH_3) %>% 
  left_join(GH_total, by = "date") %>%
  rename(`Entire home/apt` = FREH_3, `Private room` = GH) %>%
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") %>% 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

# STR-induced housing loss ------------------------------------------------

#' At the end of 2019, there were 2,330 [1] FREH listings in the City of 
#' Vancouver, and 230 [2] more housing units which were operating as ghost 
#' hostels. In total, therefore, short-term rentals removed 2,560 [3] housing 
#' units from Vancouver’s long-term market last year (Figure 3.1). Notably, while 
#' the number of active daily listings declined by 3.0% over 2019, the number of 
#' housing units which STRs took off of Vancouver’s housing market increased by 
#' 41.2% [4] in that same time period, from 1,810 [5] to 2,560. The peak in housing loss
#' remains in 2017, with 2720 units of housing lost due to STR in October 2017.

#' [1] FREH in 2019-1
FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3) %>% round(digit = -1)

#' [2] GH in 2019-12
GH %>% filter(date == LTM_end_date) %>% pull(housing_units) %>% sum() %>% 
  round(digit = -1)

#' [3] Total housing loss for 2019
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date) %>% pull(housing_units) %>% sum()}} %>% 
  round(digit = -1)

#' [4] YOY increase in housing loss
{{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date) %>% pull(housing_units) %>% sum()}} /
  {{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
      {GH %>% filter(date == LTM_end_date - years(1)) %>% 
          pull(housing_units) %>% sum()}}} %>% 
  {. - 1} %>% 
  round(3)

#' [5] Total housing loss for 2018
{{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date - years(1)) %>% pull(housing_units) %>% 
        sum()}} %>% 
  round(digit = -1)

#' [5] Total housing loss for 2019
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date ) %>% pull(housing_units) %>% 
        sum()}} %>% 
  round(digit = -1)

#' [6] Peak in housing loss
housing_loss %>% 
  group_by(date) %>% 
  summarize(units = round(sum(`Housing units`), -1)) %>% 
  arrange(desc(units)) %>% 
  slice(1:5)

#' At the end of 2019 close to two thirds (65.9% [1]) of entire-home listings 
#' and more than four out of ten (42.0% [1]) private-room listings were taking housing off 
#' the market in Montreal (Figure 3.2). Three years earlier, the proportions 
#' were only 39.3% [1] and 26.7% [1] respectively.

#' [1] Housing loss shares
daily %>% 
  filter(housing, status != "B", listing_type %in% c("Entire home/apt",
                                                     "Private room")) %>% 
  group_by(date) %>% 
  summarize(eh = mean(FREH_3[listing_type == "Entire home/apt"] > 0.5),
            pr = mean(GH[listing_type == "Private room"])) %>% 
  mutate(across(c(eh, pr), slide_dbl, mean, .before = 30)) %>% 
  filter(date %in% as.Date(c("2016-12-31", "2019-12-31")))

#' The 2,560 housing units taken off of Vancouver’s housing market in 2019 is 
#' only 0.8% [1] of the total amount of housing in the city, but this housing 
#' loss has been concentrated in a small part of the city.... In the Downtown 
#' area, 1.8% [2] of all housing units have been converted to 
#' dedicated STRs, while the figure is 1.6% [2] for Riley Park. In 
#' Ville-Marie the rental vacancy rate was 2.5% [3] in 2019, while it was 
#' 1.5% [3] in Le Plateau-Mont-Royal in 2018 (CMHC did not release a number for 
#' 2019). This means that there are more dedicated STRs in these neighbourhoods 
#' than there are vacant apartments for rent.

#' [1] Total housing loss as % of dwellings
{{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == LTM_end_date) %>% pull(housing_units) %>% sum()}} /
  sum(LA$dwellings)} %>% 
  round(3) %>% 
  scales::percent(0.1)

#' [2] Housing loss in Downtown, Riley Park, and Shaughnessy
LA %>% 
  left_join(FREH_area) %>% 
  left_join(GH_area) %>% 
  filter(date == "2019") %>% 
  mutate(GH = if_else(is.na(GH), 0L, GH),
         housing_loss_per_dwelling = (FREH + GH) / dwellings,
         housing_loss_per_dwelling = round(housing_loss_per_dwelling, 3)) %>% 
  st_drop_geometry() %>% 
  filter(area %in% c("Downtown", "Riley Park", "Shaughnessy"))

#' [3] Vacancy rates in Downtown and Riley Park
annual_vacancy %>% 
  filter(dwelling_type == "Total",
         bedroom == "Total",
         zone %in% c(3),
         !is.na(vacancy)) %>% 
  group_by(zone) %>% 
  filter(date == max(date)) %>% 
  ungroup() 

#' Table 3.1
LA_housing_table <- 
  LA %>% 
  st_drop_geometry() %>% 
  left_join(FREH_area) %>% 
  left_join(GH_area) %>%
  mutate(GH = if_else(is.na(GH), 0L, GH)) %>% 
  group_by(area) %>% 
  summarize(
    dwellings = mean(dwellings),
    housing_loss_2018 = sum(FREH[date == 2018]) + sum(GH[date == 2018]),
    housing_loss_2019 = sum(FREH[date == 2019]) + sum(GH[date == 2019]),
    yoy_change = (housing_loss_2019 - housing_loss_2018) / housing_loss_2018,
    housing_loss_pct_2019 = housing_loss_2019 / dwellings)
  
LA_housing_table %>% 
  summarize(
    dwellings = sum(dwellings),
    housing_loss_2018 = sum(housing_loss_2018),
    housing_loss_2019 = sum(housing_loss_2019),
    yoy_change = (housing_loss_2019 - housing_loss_2018) / housing_loss_2018,
    housing_loss_pct_2019 = housing_loss_2019 / dwellings) %>% 
  mutate(area = "City of Vancouver") %>% 
  bind_rows(LA_housing_table) %>% 
  relocate(area) %>% 
  select(-dwellings) %>% 
  filter(housing_loss_2019 > 50) %>% 
  mutate(across(c(housing_loss_2018, housing_loss_2019), round, -1)) %>% 
  mutate(across(c(yoy_change, housing_loss_pct_2019), round, 4)) %>% 
  arrange(desc(housing_loss_2019)) %>%
  rename(Area = area,
         `Housing loss (2019)` = housing_loss_2019,
         `Housing loss (2018)` = housing_loss_2018,
         `Year-over-year growth (%)` = yoy_change,
         `% of housing lost (2019)` = housing_loss_pct_2019) %>% 
  gt() %>% 
  tab_header(title = "STR-induced housing loss by area") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(4, 5), decimals = 1) %>% 
  fmt_number(columns = 2:3, decimals = 0)



# The impact of STRs on rental housing supply and vacancy rates -----------

#' In 2019, the City of Montreal’s rental vacancy rate declined for the third 
#' year in a row, reaching a 15-year low of 1.6% [1]. According to CMHC, a 
#' healthy rental market should have a vacancy rate of at least 3%, but most 
#' neighbourhoods in the central city are significantly below that. Le Sud-Ouest 
#' had the city’s lowest vacancy rate, at 0.3% [2], which means that, of the 
#' zone’s approximately 31,000 [3] rental apartments, fewer than 100 [4] were 
#' available to be rented by prospective tenants in October 2019, when CMHC’s 
#' survey was conducted. In general, vacancy rates are even lower for 
#' family-sized housing units (defined by CMHC as units with two or more 
#' bedrooms). For example, the vacancy rate for units with three or more 
#' bedrooms was 0.5% [5] for Ville-Marie (the Downtown Montreal/Îles-des-Soeurs 
#' zone), and 0.3% [6] in Le Plateau-Mont-Royal. 

#' [1] 2019 city vacancy
city_vacancy %>% 
  filter(bedroom == "Total") %>% 
  arrange(desc(date))

#' [2] Le Sud-Ouest 2019 vacancy
annual_vacancy %>% 
  # I added quality a, if not we have 2 southeast vancouver
  filter(date == 2019, dwelling_type == "Total", bedroom == "Total", quality == "a") %>% 
  filter(vacancy <= 0.03)

#' [3] Total rental units in Downtown
annual_units %>% 
  filter(zone_name == "Downtown", 
         date == 2019, dwelling_type == "Total", 
         bedroom == "Total") %>% 
  pull(units) %>%
  round(-2)

#' [4] Total vacant units in Downtown
annual_units %>% 
  left_join(annual_vacancy) %>% 
  filter(zone_name == "Downtown", 
         date == 2019, dwelling_type == "Total", 
         bedroom == "Total") %>% 
  transmute(vacant_units = vacancy * units)

#' [5] 3+ bedroom vacancy rate in Downtown
annual_vacancy %>% 
  filter(zone_name == "Downtown", 
         date == 2019, dwelling_type == "Total",
         bedroom == "3 Bedroom +") %>% 
  pull(vacancy)

#' [6] 3+ bedroom vacancy rate in LPM
annual_vacancy %>% 
  filter(zone == 6, date == 2019, dwelling_type == "Total",
         bedroom == "3 Bedroom +") %>% 
  pull(vacancy)

#' For example, in the Notre-Dame-de-Grâce/Côte-St-Luc zone, 66.9% [1] of 
#' households are renters, so we assume that 66.9% of housing units converted to 
#' dedicated STRs would have been rental housing, and the remaining 33.1% would 
#' have been ownership housing.

cmhc %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(p_renter = renter_households / total_households) %>% 
  select(zone_name, p_renter)


# The impact of STRs on residential rents ---------------------------------

#' Between 2015 and 2019, we estimate that STRs have been responsible for a 
#' 2.4% [1] increase in the average monthly rent. As average rents have risen 
#' 16.9% [2] in this time period, this implies that one seventh of all rent 
#' increases over the last five years have been caused by the growth of STRs. 
#' Put differently, from 2015 to 2019, the average renter household in Montreal 
#' has paid an additional $870 [3] in rent because of the impact of STRs on the 
#' housing market.

#' [1] Total rent increase
rent_increase %>% 
  slice(-1) %>% 
  mutate(rent_increase = rent_increase + 1) %>% 
  pull(rent_increase) %>% 
  prod() %>% 
  {. - 1} %>% 
  round(3)

# Or this way? TKTK
rent_increase %>% 
  filter(!is.na(rent_increase)) %>% 
  summarize(total = prod(1 + rent_increase)) %>% 
  pull(total)

#' [2] Average rent increase
city_avg_rent %>% 
  filter(bedroom == "Total") %>% 
  filter(date >= 2014) %>% 
  mutate(increase = slide_dbl(avg_rent, ~{.x[2] / .x[1]}, .before = 1)) %>% 
  slice(-1) %>% 
  pull(increase) %>% 
  prod() %>% 
  {. - 1} %>% 
  round(3)

#' [3] Extra rent payment
city_avg_rent %>% 
  filter(bedroom == "Total") %>% 
  mutate(date = as.character(date)) %>% 
  left_join(rent_increase, by = c("date" = "year_created")) %>% 
  filter(date >= "2015") %>% 
  select(date, avg_rent, rent_increase) %>% 
  mutate(rent_plus_1 = 1 + rent_increase,
         cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
         extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
  pull(extra_rent) %>% 
  sum() %>% 
  round(-1) %>% 
  scales::dollar()

#' Table 3.2
strs_by_zone <- 
  property %>% 
  st_intersection(cmhc) %>% 
  st_drop_geometry() %>% 
  select(property_ID, zone) %>% 
  left_join(daily, .) %>% 
  filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
         status != "B") %>% 
  count(zone) %>% 
  mutate(active_strs = n / 365)

annual_avg_rent %>% 
  filter(bedroom == "Total", occupied_status == "Occupied Units") %>% 
  select(-bedroom, -occupied_status, -quality, -occ_rent_higher) %>% 
  mutate(date = as.character(date)) %>% 
  left_join(rent_increase_zone, by = c("zone", "date" = "year_created")) %>% 
  group_by(zone, zone_name) %>%
  mutate(rent_plus_1 = 1 + rent_increase,
         cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
         extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
  summarize(avg_rent_2015 = avg_rent[date == 2015],
            avg_rent_2019 = avg_rent[date == 2019],
            rent_change = (avg_rent_2019 - avg_rent_2015) / avg_rent_2015,
            str_rent_increase = prod(rent_plus_1) - 1,
            str_share = str_rent_increase / rent_change,
            extra_rent = sum(extra_rent)) %>% 
  ungroup() %>% 
  left_join(strs_by_zone) %>% 
  relocate(active_strs, .after = zone_name) %>% 
  select(-zone, -n) %>% 
  add_row(
    tibble_row(
      zone_name = "City of Vancouver",
      active_strs = nrow(
        filter(daily, housing, status != "B", date >= LTM_start_date, 
               date <= LTM_end_date)) / 365,
      avg_rent_2015 = (filter(city_avg_rent, date == 2015, 
                              bedroom == "Total"))$avg_rent,
      avg_rent_2019 = (filter(city_avg_rent, date == 2019, 
                              bedroom == "Total"))$avg_rent,
      rent_change = (avg_rent_2019 - avg_rent_2015) / avg_rent_2015,
      str_rent_increase = (rent_increase %>% 
                             slice(-1) %>% 
                             mutate(rent_increase = rent_increase + 1) %>% 
                             pull(rent_increase) %>% 
                             prod()) - 1,
      str_share = str_rent_increase / rent_change,
      extra_rent = city_avg_rent %>% 
        filter(bedroom == "Total") %>% 
        mutate(date = as.character(date)) %>% 
        left_join(rent_increase, by = c("date" = "year_created")) %>% 
        filter(date >= "2015") %>% 
        select(date, avg_rent, rent_increase) %>% 
        mutate(rent_plus_1 = 1 + rent_increase,
               cumulative_increase = slide_dbl(rent_plus_1, prod, .before = 4),
               extra_rent = (cumulative_increase - 1) * avg_rent * 12) %>% 
        pull(extra_rent) %>% 
        sum())) %>% 
  arrange(-active_strs) %>% 
  slice(1:9) %>% 
  mutate(across(c(active_strs, avg_rent_2015, avg_rent_2019, extra_rent), round, 
                -1)) %>% 
  set_names(c("CMHC Zone", "Active daily STR listings (2019)", 
              "Average rent (2015)", "Average rent (2019)",
              "Total rent increase (2015-2019)", 
              "STR-induced rent increase (2015-2019)",
              "STR share of total rent increase",
              "Average extra rent paid due to STRs (2015-2019")) %>% 
  gt() %>%
  tab_header(title = "STR impacts on rents") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(5:7), decimals = 1) %>% 
  fmt_number(columns = 2:4, decimals = 0) %>% 
  fmt_currency(8, decimals = 0)


# Clean up ----------------------------------------------------------------

rm(annual_avg_rent, annual_units, annual_vacancy, area_housing_table,
   LA, LA_raw, city, city_avg_rent, city_units, city_vacancy,
   cmhc, DA, DA_probabilities_2017, DA_probabilities_2019, FREH, FREH_area,
   GH_area, listing_probabilities_2017, listing_probabilities_2019, province,
   rent_increase, rent_increase_zone, streets, streets_downtown, strs_by_zone)
