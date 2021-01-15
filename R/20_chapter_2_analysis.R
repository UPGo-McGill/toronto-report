#### 20 CHAPTER 2 ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 2. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `09_str_processing.R`
#' - `10_national_comparison.R`
#' - `13_raffle_condo.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

qload("output/str_processed.qsm", nthreads = availableCores())
load("output/national_comparison.Rdata")
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/FREH_model.qsm", nthreads = availableCores())


# Prepare new objects -----------------------------------------------------

# 2019 active
active_2019 <- 
  daily %>%
  filter(housing, status %in% c("R", "A"), date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  pull(property_ID) %>% 
  unique()
  
# 2019 revenue
revenue_2019 <-
  daily %>%
  filter(housing, status == "R", date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .)


# Active daily listings ---------------------------------------------------

#'  In 2019 there was an average of 3,970 [1] active daily listings (Figure 2.1) 
#'  operated by an average of 2,700 [2] hosts. These hosts collectively earned 
#'  $151.8 million [3] in 2019—an average of $38,300 [4] per daily active 
#'  listing or $56,300 [5] per active host. There was also a daily average of 
#'  2,540 [1] listings which were visible on the Airbnb and VRBO websites but 
#'  were blocked by the host from receiving reservations. The presence of these 
#'  listings can erroneously suggest that a city’s STR market is larger than it 
#'  is. When these blocked but inactive listings are included, the 
#'  average listing earned $15,600 [6] last year, and the average host earned 
#'  $26,000 [7]. Finally, there was a daily average of 90 [8] listings that 
#'  were not located in private housing units (B&Bs, hotels, etc.), which 
#'  have been excluded from the analysis in this report. 
#'  
#'  Active daily listings peaked in the beginning of September 2017 [9] at 5,180 [9], and have 
#'  since declined. There were 3.0% [10] fewer listings active on average in 
#'  2019 than in 2018. However, host revenue followed the opposite pattern, 
#'  increasing by 1.4% [10] between 2018 and 2019. These facts point to an 
#'  increasingly commercializing STR market, where the number of listings is 
#'  relatively stable but a rising proportion of these listings are operated on 
#'  a full-time basis.

#' [1] Average active and blocked daily listings in 2019
daily %>% 
  filter(housing, date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date, B = status == "B") %>% 
  group_by(B) %>% 
  summarize(round(mean(n), digit = -1))

#' [2] Average number of hosts (taking out blocked 365 days)
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(hosts = round(mean(n), digit = -1))

#' [3] Total annual revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [4] Average revenue per active listing
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [5] Average revenue per active host
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [6] Average revenue per all listings
prettyNum(round(mean(revenue_2019$revenue_LTM), digit = -2), ",")

#' [7] Average revenue per all hosts
revenue_2019 %>% 
  st_drop_geometry() %>%
  filter(!is.na(host_ID)) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize("avg_host_rev" = prettyNum(round(mean(host_rev), digit = -2), ","))

#' [8] Non-housing active listings
daily %>% 
  filter(!housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(date) %>% 
  summarize(non_housing = round(mean(n), digit = -1))

#' [9] Date and amount of highest activity
daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  filter(n == max(n)) %>% 
  group_by(date) %>% 
  summarize(daily_max = round((n), digit = -1))

#' [10] Active listing YOY change
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(active_listings = n() / 365,
            revenue = sum(price[status == "R"])) %>% 
  summarize(across(c(active_listings, revenue), ~{(.x[2] - .x[1]) / .x[1]}))


# STR growth rates --------------------------------------------------------

#' Overall, the year-over-year change in average active listings from 2016 to 
#' 2017 (12 months) was 19.4% [1], the year-over-year change from 2017 to 2018 
#' was -6.2% [2], and the year-over-year change from 2018 to 2019 was -3.0% [3]. 
#' In the first half of 2020, active listings fell faster thanks to the 
#' COVID-19 pandemic. The year-over-year change in active daily listings for 
#' 2020 so far (January to July) is -9.5% [4].
#' 
#' Despite there being fewer active listings in 2019 than in 2018, the number of 
#' reserved nights increased by 4.5% [5], from 0.88 million [6] reserved nights 
#' to 0.92 million [6] reserved nights, while revenue increased 1.5% [7]. Revenue 
#' from January to July 2020 is down 50.1% [8] compared to the same 
#' time last year).

#' [1] YOY listing growth, 2016-2017
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(3),
         date <= LTM_end_date - years(2)) %>% 
  group_by(year_2017 = date >= LTM_start_date - years(2)) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [2] YOY listing growth, 2017-2018
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(2),
         date <= LTM_end_date - years(1)) %>% 
  group_by(year_2018 = date >= LTM_start_date - years(1)) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [3] YOY listing growth, 2018-2019
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [4] YOY listing growth, 2019-2020
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date,
         date <= LTM_end_date + years(1),
         (date <= max(daily$date) - years(1) | date >= LTM_start_date + years(1)),
         date != "2020-02-29") %>%
  group_by(year_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize(n = n() / as.numeric((max(daily$date) - as.Date("2020-01-01")))) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [5] YOY reservation change, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n()) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [6] Reservation counts, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n())

#' [7] YOY revenue change, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])

#' [8] YOY revenue change, 2019-2020
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date,
         date <= LTM_end_date + years(1),
         (date <= "2019-07-31" | date >= LTM_start_date + years(1)),
         date != "2020-02-29") %>%
  group_by(year_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])


# Vancouver in comparison with other major Canadian cities -----------------

#' In 2019, Vancouver had the second largest STR market in the country by both 
#' active listing numbers (3,920 [1]) and host revenue ($151.8 million [2]), 
#' falling in both cases behind Toronto and Montreal (Table 2.2). However, in relative terms 
#' Vancouver stands considerably ahead of both Montreal and Toronto. Vancouver 
#' had the most active listings per 1000 households (12.3 [3] compared to 
#' 10.7 [3] in Montreal) and the most revenue per listing ($38,700 [4] compared 
#' to $27,200 [4] in Toronto).

#' [1] Daily active listings
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(active_listings = round(mean(n), digit = -1))

#' [2] Annual host revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [3] Vancouver and Montreal listings per 1000 households
national_comparison %>% 
  filter(city %in% c("Vancouver", "Montreal")) %>% 
  select(city, listings_per_1000)

#' [4] Vancouver and Toronto revenue per listing
national_comparison %>% 
  filter(city %in% c("Vancouver", "Toronto")) %>% 
  select(city, revenue_per_listing)

(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' Table 2.1
national_comparison %>% 
  mutate(active_daily_listings = prettyNum(round(active_daily_listings, -1), 
                                           ","),
         listings_per_1000 = round(listings_per_1000, 1),
         revenue = prettyNum(round(revenue, -5), ","),
         revenue_per_listing = prettyNum(round(revenue_per_listing, -2), ","))


# Location of STR listings and revenue ------------------------------------

LA_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  group_by(date, area) %>% 
  summarize(n = n(),
            revenue = sum(price[status == "R"])) %>% 
  left_join(st_drop_geometry(LA)) %>% 
  group_by(area, dwellings) %>% 
  summarize(active_listings = mean(n[date >= LTM_start_date]),
            active_2018 = mean(n[date < LTM_start_date]),
            active_growth = (active_listings - active_2018) / active_2018,
            annual_rev = sum(revenue[date >= LTM_start_date]),
            rev_2018 = sum(revenue[date < LTM_start_date]),
            rev_growth = (annual_rev - rev_2018) / rev_2018,
            .groups = "drop") %>% 
  mutate(listings_pct = active_listings / sum(active_listings, na.rm = TRUE),
         listings_pct_dwellings = active_listings / dwellings,
         rev_pct = annual_rev / sum(annual_rev)) %>% 
  select(area, active_listings, active_growth, listings_pct,
         listings_pct_dwellings, annual_rev, rev_pct, rev_growth)

LA_breakdown %>% 
  arrange(desc(active_listings)) %>% 
  ggplot() +
  geom_line(aes(active_listings, active_growth))

#' STR activity in Vancouver is mostly concentrated in the area of Downtown (Table 2.2). 
#' This area accounts for 25.4% [1] of all listings in 2019, and even higher shares of 
#' host revenue (32.8% [1]). The area with the next highest percentage of average number 
#' of daily active listings is Kitsilano (7.9% [2]), followed by West End (6.7% [2]). The 
#' former accounts for 8.9% [2] of annual STR revenue in the city and the latter 7.7% [2].
#' 
#' When measured in per-capita terms, areas are fairly distributed. Downtown is 
#' showing the highest figure of active STR listings on total housing units at 
#' 2.5% [1]. At second place, active STR listings account for 2.3% [3] of 
#' Riley park’s housing units, while all the other area’s figures are under 2%.

#' [1] Figures for Downtown
LA_breakdown %>% 
  slice(2)

#' [2] Figures for Kitsilano and West End
LA_breakdown %>% 
  slice(c(10, 21))

#' [3] Figures for Riley Park
LA_breakdown %>% 
  slice(c(15))

#' Table 2.2
LA_breakdown %>% 
  select(area, active_listings, active_growth, listings_pct_dwellings,
         annual_rev, rev_growth) %>% 
  set_names(c("Area",
              "Daily active listings (average)",
              "Active listing year-over-year growth rate",
              "Active listings as % of dwellings",
              "Annual revenue (CAD)",
              "Annual revenue growth")) %>% 
  filter(`Daily active listings (average)` > 100) %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = 
           round(`Daily active listings (average)`, digit = -1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
         `Annual revenue (CAD)` = 
           paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
                  str_sub(`Annual revenue (CAD)`, -6, -6), " million")) %>%
  gt() %>% 
  tab_header(
    title = "Area breakdown",
    subtitle = "Areas with more than 100 daily active listings average, 2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(3:4, 6), decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)

# Need to add Vancouver row, with % listings per dwelling
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(active_listings = round(mean(n), digit = -1)) %>% 
  pull(active_listings) %>% 
  {. / sum(LA$dwellings)}


# Listing types and sizes -------------------------------------------------

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= LTM_start_date,
         date <= LTM_end_date) %>% 
  group_by(listing_type) %>% 
  summarize(
    active_listings = n() / 365,
    revenue = sum(price[status == "R"]),
    .groups = "drop") %>% 
  mutate(pct_of_listings = active_listings / sum(active_listings),
         pct_of_revenue = revenue / sum(revenue))

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date - years(1)) %>% 
  group_by(listing_type) %>% 
  summarize(active_2018 = n() / 365) %>% 
  left_join(listing_type_breakdown, .) %>% 
  mutate(pct_listing_growth = (active_listings - active_2018) / active_2018) %>% 
  select(-active_2018)

#' The majority of STRs in Vancouver are entire homes, a category which 
#' includes single-family homes, townhouses, apartments and condominiums. 
#' 42.1% [1] of these were one-bedroom housing units, and a third (33.3% [1]) were 
#' two-bedrooms units. Almost a fifth of these listings (17.9% [1]) were 3 or 
#' more bedrooms housing units, and 6.7% [1] were studios. 

#' In 2019 entire-home listings accounted for 69.8% [2] of all daily active 
#' listings, and 86.4% [2] of total host revenue. Private rooms accounted for 
#' nearly all of the remainder.

#' [1] Bedroom counts
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% active_2019, listing_type == "Entire home/apt") %>% 
  mutate(bedrooms = if_else(bedrooms >= 3, "3+", as.character(bedrooms))) %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n))

#' [2] EH listings and revenue
listing_type_breakdown %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(-active_listings, -revenue)

#' Table 2.3
listing_type_breakdown %>%
  mutate(active_listings = prettyNum(round(active_listings, digits = -1), ","),
         revenue = scales::dollar(revenue, 0.1, 1 / 1000000),
         pct_of_listings = scales::percent(pct_of_listings, 0.1),
         pct_of_revenue = scales::percent(pct_of_revenue, 0.1),
         pct_listing_growth = scales::percent(pct_listing_growth, 0.1)) %>% 
  rename(`Listing type` = listing_type,
         `Active listings` = active_listings,
         `Annual revenue (million)` = revenue,
         `Share of all active listings` = pct_of_listings,
         `Share of all annual revenue` = pct_of_revenue,
         `Average daily listing growth (year-over-year)` = pct_listing_growth
         ) %>% 
  gt() %>% 
  tab_header(
    title = "Listing type breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 1) %>% 
  fmt_number(columns = 2, decimals = 0) %>% 
  fmt_currency(columns = 3, decimals = 0)


# STRs and housing tenure -------------------------------------------------

active_tenure_2017 <- 
  daily %>% 
  filter(housing, date >= "2017-01-01", date <= "2017-12-31", status != "B") %>% 
  left_join(listing_probabilities_2017) %>% 
  group_by(date, area) %>% 
  summarize(n_listings_2017 = n(),
            n_condo_2017 = as.numeric(sum(condo, na.rm = TRUE)),
            n_renter_2017 = sum(p_renter, na.rm = TRUE)) %>% 
  group_by(area) %>% 
  summarize(across(where(is.numeric), mean))

active_tenure_2019 <- 
  daily %>% 
  filter(housing, date >= LTM_start_date, 
         date <= LTM_end_date, status != "B") %>% 
  left_join(listing_probabilities_2019) %>% 
  group_by(date, area) %>% 
  summarize(n_listings_2019 = n(),
            n_condo_2019 = as.numeric(sum(condo, na.rm = TRUE)),
            n_renter_2019 = sum(p_renter, na.rm = TRUE)) %>% 
  group_by(area) %>% 
  summarize(across(where(is.numeric), mean))

area_tenure <- 
  DA_probabilities_2019 %>% 
  mutate(across(c(p_condo, p_renter), ~{.x * dwellings})) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  select(p_condo, p_renter, geometry) %>% 
  st_interpolate_aw(LA, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(-Group.1) %>% 
  rename(n_condo = p_condo, n_renter = p_renter) %>% 
  cbind(LA, .) %>% 
  as_tibble() %>% 
  st_as_sf()

tenure_breakdown <-
  area_tenure %>% 
  left_join(active_tenure_2017) %>% 
  left_join(active_tenure_2019) %>% 
  relocate(geometry, .after = last_col()) %>%
  transmute(area, n_condo_2017, n_renter_2017, n_condo_2019, n_renter_2019,
            condo_pct_2017 = n_condo_2017 / n_listings_2017,
            condo_pct_2019 = n_condo_2019 / n_listings_2019,
            renter_pct_2017 = n_renter_2017 / n_listings_2017,
            renter_pct_2019 = n_renter_2019 / n_listings_2019)

#' Of all the listings active during 2019, 11.2% [1] were identified as 
#' condominiums in this way, making condominiums the second most common 
#' property type in Vancouver. The overwhelming majority (73.5% [1]) were 
#' identified as “Apartment”, and most of the rest were either “House” 
#' (5.5% [1]) or “Loft” (4.6% [1]).... There are 12 [2] dissemination LA in 
#' Vancouver in which condominiums are more than 95% of the housing stock. 
#' These 12 LA contain 114 [3] active STR listings, which by definition must 
#' be nearly entirely condominiums. And yet only 44.7% [4] of these listings are 
#' described as condominiums by their hosts; 39.5% [4] are described as 
#' apartments and 6.1% [4] are described as lofts. In fact, the correlation 
#' between the proportion of condominiums in a dissemination area and the 
#' proportion of listings in the area which self-describe as condominiums is 
#' -0.01 [5]—completely random.

#' [1] Active listing property types
property %>% 
  filter(property_ID %in% active_2019) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n))

#' [2] High-condo DAs
high_condos <- 
  DA_probabilities_2019 %>% 
  filter(p_condo >= 0.95) %>% 
  pull(GeoUID)

length(high_condos)

#' [3] Listings in high-condo DAs
property %>% 
  filter(property_ID %in% active_2019, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  nrow()

#' [4] Property types in high-condo DAs
property %>% 
  filter(property_ID %in% active_2019, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n))

#' [5] Correlation between condo % and condo property_type
property %>% 
  filter(property_ID %in% active_2019) %>% 
  st_drop_geometry() %>% 
  count(GeoUID, property_type) %>% 
  group_by(GeoUID) %>% 
  summarize(condo_pct = n[property_type == "Condominium"] / sum(n)) %>% 
  left_join(DA_probabilities_2019, .) %>% 
  st_drop_geometry() %>% 
  summarize(cor = cor(p_condo, condo_pct, use = "complete.obs"))

#' Table 2.4
tenure_breakdown %>% 
  st_drop_geometry() %>% 
  mutate(listings_2017 = n_condo_2017 / condo_pct_2017,
         listings_2019 = n_condo_2019 / condo_pct_2019) %>% 
  summarize(
    area = "City of Vancouver",
    n_condo_2017 = sum(n_condo_2017),
    n_renter_2017 = sum(n_renter_2017),
    n_condo_2019 = sum(n_condo_2019),
    n_renter_2019 = sum(n_renter_2019),
    condo_pct_2017 = n_condo_2017 / sum(listings_2017),
    condo_pct_2019 = n_condo_2019 / sum(listings_2019),
    renter_pct_2017 = n_renter_2017 / sum(listings_2017),
    renter_pct_2019 = n_renter_2019 / sum(listings_2019)) %>% 
  bind_rows(st_drop_geometry(tenure_breakdown)) %>% 
  mutate(`Number of STRs in condos` = round(n_condo_2019, digits = 1),
         `% of STRs in condos (2019)` = condo_pct_2019,
         `% change in % of STRs in condos (2017 to 2019)` =
           (condo_pct_2019 - condo_pct_2017) / condo_pct_2017,
         `Number of STRs in rental units` = round(n_renter_2019, digits = 1),
         `% of STRs in rental units (2019)` = renter_pct_2019,
         `% change in % of STRs in rental units (2017 to 2019)` =
           (renter_pct_2019 - renter_pct_2017) / renter_pct_2017) %>% 
  select(-c(n_condo_2017:renter_pct_2019)) %>% 
  rename(area = area) %>% 
  arrange(desc(`Number of STRs in condos`)) %>%
  slice(1:12) %>% 
  mutate(`Number of STRs in condos` = round(`Number of STRs in condos`, 
                                            digit = -1),
         `Number of STRs in rental units` = 
           round(`Number of STRs in rental units`, digit = -1)) %>%
  gt() %>% 
  tab_header(title = "Tenure breakdown",
             subtitle = "STRs in condominiums and rental units by area") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(3:4, 6:7), decimals = 1) %>% 
  fmt_number(columns = c(2, 5), decimals = 0)


# Revenue distribution ----------------------------------------------------

host_rev <-
  daily %>%
  filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
         status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price))
  
#' Among all the STR hosts who earned revenue in the City of Vancouver last year, 
#' the median revenue was $15,700 [1], while the top host (in this case a network 
#' of numerous host accounts which we discuss below) earned $4.1 million [2] 
#' (Table 2.5). Throughout the City of Vancouver, there were 27 hosts [3] that 
#' earned more than $250,000 in 2019. Figure 2.6 shows the percentage of the 
#' total $151.8 million [4] in STR revenue which accrued to each decile of 
#' hosts. The most successful 10% of hosts earned more four tenths
#' (43.2% [5]) of all STR revenue. The revenue concentration is even steeper 
#' among the top 10%: the top 5% earned 30.5% [6] of revenue, while the top 13.6% 
#' of hosts earned 40.7% [7] of all revenue. 

#' [1] Median host revenue
median(host_rev$rev) %>% round(-2)

#' [2] Top earner
max(host_rev$rev) %>% round(-5)

#' [3] Hosts above $250,000
host_rev %>% 
  filter(rev >= 250000) %>% 
  nrow()

#' [4] Total annual revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [5] Top 10% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .9)]) / sum(rev))
  
#' [6] Top 5% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .95)]) / sum(rev))

#' [7] Top 1% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .99)]) / sum(rev))

#' Table 2.5
host_rev %>% 
  pull(rev) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate(across(1:3, scales::dollar, accuracy = 1000, big.mark = ",")) %>% 
  mutate(`100th percentile` = scales::dollar(`100th percentile`, 0.1, 
                                             scale = 1 / 1000000, 
                                             suffix = " million")) %>% 
  pivot_longer(everything(), names_to = "Host percentile", 
               values_to = "Annual revenue") %>% 
  gt() %>% 
  tab_header(
    title = "Host income",
  ) %>%
  opt_row_striping() 


# Multilistings -----------------------------------------------------------

commercial_listings <- 
  daily %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()


#' Since 94.0% [1] of entire-home listings have three or fewer bedrooms...

#' [1] Bedrooms
property %>% 
  filter(housing, created <= LTM_end_date, scraped >= LTM_start_date, 
         listing_type == "Entire home/apt", !is.na(bedrooms)) %>% 
  st_drop_geometry() %>% 
  summarize(bed_3 = mean(bedrooms <= 3)) %>% 
  pull(bed_3) %>% 
  scales::percent(0.1)

#' In 2019, 34.6% [1] of active listings in Vancouver were multilistings, earning 
#' 30.3% [2] of total host revenue. Multilistings have been a steadily growing 
#' share of both listings and revenue in Vancouver since 2017 (Figure 2.7), and 
#' amidst generally declining STR activity during the COVID-19 pandemic,
#' multilistings briefly earned slightly more than 1 out of every 4 [3] dollars 
#' on STR platforms in Vancouver.

#' [1] 2019 ML listings
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(multi) %>% 
  summarize(multi_listings = n[2] / sum(n))

#' [2] 2019 ML revenue
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date, 
       date <= LTM_end_date) %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n))

#' [3] June 2020 ML revenue
daily %>% 
  filter(housing, status == "R", date >= "2020-06-01", date <= "2020-06-30") %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n))

#' The pinnacle in the number of commercial listings in 2018 was 2,920 [1] in 
#' August, and this figure was reached again exactly one year 
#' later at 2,930 [2].

#' [1] Peak of commercial listings in 2018
commercial_listings %>% 
  filter(date >= "2018-01-01", date <= "2018-12-31", commercial) %>% 
  arrange(desc(n)) %>% 
  slice(c(1)) %>% 
  mutate(n = round(n, -1))

#' [2] Peak of commercial listings after it
commercial_listings %>% 
  filter(date >= "2019-01-01", commercial) %>% 
  arrange(desc(n)) %>% 
  slice(c(1)) %>%
  mutate(n = round(n, -1))


# Listings taken down due to regulations -----------------------------------

taken_down_properties <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2018-08-23", scraped <= "2018-08-31") %>% 
  pull(property_ID)

# How many units have been taken down?
length(taken_down_properties)

# How many were still active in the year it was enforced?
daily %>% 
  filter(property_ID %in% taken_down_properties,
         date >= "2018-01-01",
         status != "B") %>% 
  distinct(property_ID) %>% 
  nrow()

# How many commercial operations active in 2018
daily %>% 
  filter(housing, 
         property_ID %in% taken_down_properties,
         date >= "2018-01-01",
         FREH_3 >= 0.5 | multi) %>% 
  distinct(property_ID) %>% 
  nrow() /
  daily %>% 
  filter(property_ID %in% taken_down_properties,
         date >= "2018-01-01",
         status != "B") %>% 
  distinct(property_ID) %>% 
  nrow()

# How many commercial operations
daily %>% 
  filter(housing, 
         property_ID %in% taken_down_properties,
         date >= key_date_regulations,
         FREH_3 >= 0.5 | multi) %>% 
  distinct(property_ID) %>% 
  nrow()

# Percentage of commercial listings taken down
daily %>% 
  filter(housing, 
         property_ID %in% taken_down_properties,
         date >= key_date_regulations,
         FREH_3 >= 0.5 | multi) %>% 
  distinct(property_ID) %>% 
  nrow() /
commercial_listings %>% 
  filter(date == key_date_regulations, commercial) %>% 
  pull(n)


# Clean up ----------------------------------------------------------------

rm(active_tenure_2017, active_tenure_2019, area_tenure, LA,
   LA_breakdown, LA_raw, city, commercial_listings, DA,
   DA_probabilities_2017, DA_probabilities_2019, host_rev, 
   listing_probabilities_2017, listing_probabilities_2019, 
   listing_type_breakdown, national_comparison, province, revenue_2019,
   streets, streets_downtown, tenure_breakdown, active_2019, high_condos)
