#### CoV vs CMA CHAPTER ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 2. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `09_str_processing.R`
#' - `12_FREH_model.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(caret)

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/FREH_model.qsm", nthreads = availableCores())
qload("output/str_bc_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())


# Prepare new objects -----------------------------------------------------

# Active listings Vancouver
active_listings <-
  daily %>%
  filter(housing, status != "B") %>%
  count(date, listing_type) %>%
  group_by(listing_type) %>%
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>%
  ungroup()

active_listings <-
  daily %>%
  filter(housing, status != "B") %>%
  count(date) %>%
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>%
  bind_rows(active_listings) %>%
  arrange(date, listing_type)

# Active listings BC
active_listings_bc <-
  daily_bc %>%
  filter(housing, status != "B") %>%
  count(date, listing_type) %>%
  group_by(listing_type) %>%
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>%
  ungroup()

active_listings_bc <-
  daily_bc %>%
  filter(housing, status != "B") %>%
  count(date) %>%
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>%
  bind_rows(active_listings_bc) %>%
  arrange(date, listing_type)

# Revenue 
revenue <-
  daily %>%
  filter(housing, status == "R") %>%
  group_by(date) %>%
  summarize(revenue = sum(price))

revenue_bc <-
  daily_bc %>%
  filter(housing, status == "R") %>%
  group_by(date) %>%
  summarize(revenue = sum(price))

# Commercial listings
commercial_listings <- 
  daily %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()

commercial_listings_bc <- 
  daily_bc %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()


# YOY listings, revenue and reservation growth ------------------------------------------------

#' [1] YOY listing growth, COV 2018-2019
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [2] YOY listing growth, CMA 2018-2019
daily_bc %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [3] YOY reservation change, COV 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n()) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [4] YOY revenue change, COV 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])

#' [5] YOY reservation change, CMA 2018-2019
daily_bc %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n()) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [6] YOY revenue change, CMA 2018-2019
daily_bc %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])

# Regulatory impacts: displayed listings ------------------------------------------------------

displayed_listings <-
  daily %>% 
  filter(housing) %>% 
  count(date, blocked = status == "B") %>% 
  group_by(blocked) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  group_by(date) %>% 
  mutate(n = if_else(blocked == TRUE, sum(n), n)) %>% 
  ungroup() %>% 
  mutate(blocked = if_else(blocked == TRUE, "Total listings", 
                           "Active listings"))
  
# figure_3_2_1 <- 
  displayed_listings %>% 
  ggplot(aes(date, n, colour = blocked)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  geom_line() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)]) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figures/figure_3_2_1.pdf", plot = figure_3_2_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_2_1.pdf")

# Actual and expected number of active listings after regulations -----------------------------

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date, listing_type) %>% 
  group_by(listing_type) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>% 
  bind_rows(active_listings) %>% 
  arrange(date, listing_type)

# Figure 3.2.2 Actual and trend active listings after regulations -----------------

# Create and decompose reservations time series
active_listings_trend <- 
  active_listings %>% 
  filter(listing_type == "All listings") %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(n = sum(n, na.rm = TRUE)) %>% 
  filter(yearmon <= tsibble::yearmonth("2018-07")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components()

# Get August 2017 - July 2018 seasonal
aug_jul_seasonal <- 
  active_listings_trend %>%
  slice(35:46) %>% 
  pull(seasonal)

# Get July trend
jul_trend <- 
  active_listings_trend %>% 
  slice(46) %>% 
  pull(trend)

# Apply Aug-Jul seasonal component to July trend
trends <-
  tibble(
    date = as.Date(c("2018-08-23", "2018-09-16", "2018-10-16", "2018-11-16",
                     "2018-12-16", "2019-01-16", "2019-02-16", "2019-03-16",
                     "2019-04-16", "2019-05-16", "2019-06-16", "2019-07-16",
                     "2019-08-16", "2019-09-16")),
    trend = (jul_trend + aug_jul_seasonal) / 
      c(31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30))

active_listings_trend <- 
  active_listings %>% 
  filter(listing_type == "All listings") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2016-01-01") %>% 
  left_join(trends) %>% 
  select(-listing_type) %>% 
  mutate(trend = if_else(date == "2018-08-01", n, trend)) %>%
  filter(date >= "2018-08-23", date <= "2019-09-16") %>%
  mutate(trend = zoo::na.approx(trend))

active_listings_trend <- 
  active_listings %>% 
  filter(listing_type == "All listings") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2016-01-01", date <= "2019-07-16") %>% 
  select(-listing_type) %>% 
  bind_rows(active_listings_trend) 

figure_3_2_2 <- 
  active_listings_trend %>%
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  annotate("segment", x = as.Date("2018-08-31"), xend = as.Date("2018-08-31"),
           y = -Inf, yend = Inf, alpha = 0.3)+
  annotate("curve", x = as.Date("2019-04-01"), xend = as.Date("2018-08-31") + days(10),
           y = 4850, yend = 4700, curvature = -.2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  # annotate("text", x = as.Date("2019-05-01"), y = 5000,
           # label = "Regulations", family = "Futura Condensed")+
  # geom_ribbon(aes(x = date, ymin = n, ymax = projected, group = 1),
              # data = filter(active_listings_trend, !is.na(projected)), fill = col_palette[6], 
              # alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1.5) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual active listings", "Expected with previous growth trend",
                                "Expected with no growth"), 
                     values = col_palette[c(5, 4, 3)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"),
        legend.title = element_text(face = "bold", family = "Futura",
                                    size = 10),
        legend.text = element_text( size = 10, , family = "Futura"))
  
ggsave("output/figures/figure_3_2_2.pdf", plot = figure_3_2_2, width = 8, 
         height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_2_2.pdf")

# Regulations’ impact on commercialization ----------------------------------------------------

taken_down_properties <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2018-08-23", scraped <= "2018-08-31") %>% 
  pull(property_ID)

#' [1] Number of listings removed in August 2018 that were active during the year before
daily %>% 
  filter(property_ID %in% taken_down_properties,
         date >= "2018-01-01",
         status != "B") %>% 
  distinct(property_ID) %>% 
  nrow()

#' [2] Number of listings removed in August 2018 
length(taken_down_properties)

#' [3] Number of listings considered as commercial the day they were removed
daily %>% 
  filter(housing, 
         property_ID %in% taken_down_properties,
         date >= key_date_regulations,
         FREH_3 >= 0.5 | multi) %>% 
  distinct(property_ID) %>% 
  nrow()

#' [4] Percentage of listings considered as commercial the day they were removed
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

# Figure 3_3_1
commercial_listings <- 
  daily %>% 
  filter(status != "B", date >= "2015-07-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial)

# Create and decompose reservations time series
commercial_operations <- 
  commercial_listings %>% 
  filter(commercial) %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(n = sum(n)) %>% 
  filter(yearmon <= tsibble::yearmonth("2018-07")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components()

# Get August 2017 - July 2018 seasonal
com_aug_jul_seasonal <- 
  commercial_operations %>%
  slice(26:37) %>% 
  pull(seasonal)

# Get July trend
com_jul_trend <- 
  commercial_operations %>% 
  slice(37) %>% 
  pull(trend)

# Apply Aug-Jul seasonal component to July trend
com_trends <-
  tibble(
    date = as.Date(c("2018-08-23", "2018-09-16", "2018-10-16", "2018-11-16",
                     "2018-12-16", "2019-01-16", "2019-02-16", "2019-03-16",
                     "2019-04-16", "2019-05-16", "2019-06-16", "2019-07-16",
                     "2019-08-16", "2019-09-16")),
    trend = (com_jul_trend + com_aug_jul_seasonal) / 
      c(31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30))

commercial_operations <- 
  commercial_listings %>% 
  filter(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2016-01-01") %>% 
  left_join(com_trends) %>% 
  select(-commercial) %>% 
  mutate(trend = if_else(date == "2018-08-01", n, trend)) %>%
  filter(date >= "2018-08-23", date <= "2019-09-16") %>%
  mutate(trend = zoo::na.approx(trend))

commercial_operations <- 
  commercial_listings %>% 
  filter(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2016-01-01", date <= "2019-09-30") %>% 
  select(-commercial) %>% 
  bind_rows(commercial_operations) 

figure_3_3_1 <- 
  commercial_operations %>%
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = n, ymax = trend, group = 1),
              data = filter(commercial_operations, !is.na(trend)), fill = col_palette[3], 
              alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual commercialization", "Expected commercialization"), 
                     values = col_palette[c(1, 5)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"),
        legend.title = element_text(face = "bold", family = "Futura",
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_3_3_1.pdf", plot = figure_3_3_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_3_1.pdf")


# Comparative analysis: CMA and CoV’s ---------------------------------------------------------

active_bc_2019 <- 
  daily_bc %>%
  filter(housing, status %in% c("R", "A"), date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  pull(property_ID) %>% 
  unique()

revenue_bc_2019 <-
  daily_bc %>%
  filter(housing, status == "R", date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property_bc, .)

revenue_2019 <-
  daily %>%
  filter(housing, status == "R", date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .)

#' [1] Average active and blocked daily listings in 2019 CMA
daily_bc %>% 
  filter(housing, date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date, B = status == "B") %>% 
  group_by(B) %>% 
  summarize(round(mean(n), digit = -1))

#' [2] Average number of hosts (taking out blocked 365 days) CMA
daily_bc %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(hosts = round(mean(n), digit = -1))

#' [3] Total annual revenue CMA
prettyNum(round(sum(revenue_bc_2019$revenue_LTM), digit = -5), ",")

#' [4] Average revenue per active listing CMA
(sum(revenue_bc_2019$revenue_LTM) /
    daily_bc %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [5] Average revenue per active host CMA
(sum(revenue_bc_2019$revenue_LTM) /
    daily_bc %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [6] Average revenue per active host COV
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [7] Active listings growth 2018-2019 CMA
daily_bc %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [8] Active listings growth 2018-2019 COV
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

# Comparative analysis: Regions within the CMA ------------------------------------------------------

CMA <- 
  CMA %>% 
  select(-Type) %>% 
  rename(CSD=name, dwellings=Dwellings)

CSD_breakdown <- 
  daily_bc %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  group_by(date, CSD) %>% 
  summarize(n = n(),
            revenue = sum(price[status == "R"])) %>% 
  left_join(st_drop_geometry(CMA)) %>% 
  group_by(CSD, dwellings) %>% 
  summarize(active_listings = mean(n[date >= LTM_start_date]),
            active_2018 = mean(n[date < LTM_start_date]),
            active_growth = (active_listings - active_2018) / active_2018,
            annual_rev = sum(revenue[date >= LTM_start_date]),
            rev_2018 = sum(revenue[date < LTM_start_date]),
            rev_growth = (annual_rev - rev_2018) / rev_2018,
            .groups = "drop") %>% 
  mutate(listings_pct = active_listings / sum(active_listings),
         listings_pct_dwellings = active_listings / dwellings,
         rev_pct = annual_rev / sum(annual_rev)) %>% 
  select(CSD, active_listings, active_growth, listings_pct,
         listings_pct_dwellings, annual_rev, rev_pct, rev_growth)

CSD_breakdown %>% 
  left_join(select(CMA, -dwellings), by = "CSD") %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(data=city, fill="grey75")+
  geom_sf(aes(fill = active_listings))+
  #geom_sf_text(aes(label = CSD), colour = "grey75", size = 3)+
  scale_fill_gradientn(colors = col_palette[c(2, 3, 1)])+
  theme_void() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

#' [1] Figures for Burnaby and Richmond
CSD_breakdown %>% 
  slice(c(4, 22))

#' [2] Figures for Surrey
CSD_breakdown %>% 
  slice(c(25))

#' Table 2.2
#CSD_breakdown %>% 
#  select(CSD, active_listings, active_growth, listings_pct_dwellings,
#         annual_rev, rev_growth) %>% 
#  set_names(c("CSD",
#              "daily_bc active listings (average)",
#              "Active listing year-over-year growth rate",
#              "Active listings as % of dwellings",
#              "Annual revenue (CAD)",
#              "Annual revenue growth")) %>% 
#  filter(`daily_bc active listings (average)` > 100) %>% 
#  arrange(desc(`daily_bc active listings (average)`)) %>%
#  mutate(`Active listings (average)` = 
#           round(`daily_bc active listings (average)`, digit = -1),
#         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
#         `Annual revenue (CAD)` = 
#           paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
#                  str_sub(`Annual revenue (CAD)`, -6, -6), " million")) %>%
#  gt() %>% 
#  tab_header(
#    title = "Census subdivision breakdown",
#    subtitle = "CSDs with more than 100 active listings average (except Vancouver), 2019") %>%
#  opt_row_striping() %>% 
#  fmt_percent(columns = c(3:4, 6), decimals = 1) %>% 
#  fmt_number(columns = 2,
#             decimals = 0)
#
## Need to add CMA of Vancouver row, with % listings per dwelling
#daily_bc %>% 
#  filter(housing, status != "B", date >= LTM_start_date, 
#         date <= LTM_end_date) %>% 
#  count(date) %>% 
#  summarize(active_listings = round(mean(n), digit = -1)) %>% 
#  pull(active_listings) %>% 
#  {. / sum(CMA$dwellings)}
  

# Active daily listings variation Cov vs CMA, indexed ---------------------------------------------------

# Active listings indexed
active_listings_indexed <-
  active_listings %>%
  filter(date >= key_date_regulations - years(1)) %>%
  mutate(index = 100*n/n[date == key_date_regulations]) %>%
  mutate(group = "CoV")

active_listings_bc_indexed <-
  active_listings_bc %>%
  filter(date >= key_date_regulations - years(1)) %>%
  mutate(index = 100*n/n[date == key_date_regulations]) %>%
  mutate(group = "CMA")

active_listings_both <-
  rbind(active_listings_indexed, active_listings_bc_indexed)

figure_3_4_1 <- 
  active_listings_both %>%
  filter(listing_type == "All listings") %>%
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 135, yend = 150, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-06-16"), y = 130,
           label = "COVID-19 \nAirbnb's response") + #, family = "Futura Condensed"
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 150, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations")+ #, family = "Futura Condensed"
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_4_1.pdf", plot = figure_3_4_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_4_1.pdf")


# Revenue Cov vs CMA, indexed ---------------------------------------------------

# Revenue indexed
revenue_indexed <-
  revenue %>%
  mutate(revenue = slide_dbl(revenue, mean, .before = 6, .complete = TRUE)) %>%
  filter(date >= key_date_regulations - years(1)) %>%
  mutate(index = 100*revenue/revenue[date == key_date_regulations]) %>%
  mutate(group = "CoV")

revenue_bc_indexed <-
  revenue_bc %>%
  mutate(revenue = slide_dbl(revenue, mean, .before = 6, .complete = TRUE)) %>%
  filter(date >= key_date_regulations - years(1)) %>%
  mutate(index = 100*revenue/revenue[date == key_date_regulations]) %>%
  mutate(group = "CMA")

revenue_both <-
  rbind(revenue_indexed, revenue_bc_indexed)

figure_3_4_2 <- 
  revenue_both %>%
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_4_2.pdf", plot = figure_3_4_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_4_2.pdf")


# FREH Cov vs CMA, indexed ---------------------------------------------------
#
## FREH indexed 
#FREH_indexed <- 
#  daily %>% 
#  filter(date >= key_date_regulations - years(1)) %>%
#  group_by(date) %>% 
#  summarize(n=sum(FREH_3)) %>% 
#  mutate(index = 100*n/n[date == key_date_regulations]) %>%
#  mutate(group = "CoV")
#
#FREH_bc_indexed <- 
#  daily_bc %>% 
#  filter(date >= key_date_regulations - years(1)) %>%
#  group_by(date) %>% 
#  summarize(n=sum(FREH_3)) %>% 
#  mutate(index = 100*n/n[date == key_date_regulations]) %>%
#  mutate(group = "CMA")
#
#FREH_both <- 
#  rbind(FREH_indexed, FREH_bc_indexed)
#
#FREH_both %>%
#  ggplot(aes(date, index, colour = group)) +
#  annotate("segment", x = key_date_covid, xend = key_date_covid,
#           y = -Inf, yend = Inf, alpha = 0.3) +
#  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
#           y = -Inf, yend = Inf, alpha = 0.3) +
#  geom_line(lwd = 1)+
#  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
#  scale_y_continuous(name = NULL) +
#  scale_x_date(name = NULL)+
#  theme_minimal() +
#  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())
#
#
# Commercial listings Cov vs CMA, indexed ---------------------------------------------------

# Commercial listings indexed 
commercial_listings_indexed <-
  commercial_listings %>% 
  filter(date >= key_date_regulations - years(1)) %>%
  filter(commercial == TRUE) %>% 
  mutate(index = 100*n/n[date == key_date_regulations]) %>%
  mutate(group = "CoV")

commercial_listings_bc_indexed <-
  commercial_listings_bc %>% 
  filter(date >= key_date_regulations - years(1)) %>%
  filter(commercial == TRUE) %>% 
  mutate(index = 100*n/n[date == key_date_regulations]) %>%
  mutate(group = "CMA")

commercial_listings_both <- 
  rbind(commercial_listings_indexed, commercial_listings_bc_indexed)

figure_3_4_3 <- 
  commercial_listings_both %>%
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_4_3.pdf", plot = figure_3_4_3, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_4_3.pdf")


# Spatial comparisons both sides of eastern boundary ---------------------------------------------------

# Prepare new objects
buffer_all_CoV <- st_buffer(st_cast(city,"MULTILINESTRING"),
                            1000, joinStyle = "MITRE", mitreLimit = 3)

buffer_CoV_Burnaby <- 
  st_intersection(select(st_cast(city,"MULTILINESTRING"), -everything()), 
                  select(filter(st_cast(CMA,"MULTILINESTRING"), name == "Burnaby (CY)"), -everything())) %>% 
  st_cast("LINESTRING") %>%
  st_buffer(dist = 1000, joinStyle="MITRE", mitreLimit=0.1)

# Properties within the buffer
property_buffer <- 
  property %>% 
  mutate(group = "CoV") %>% 
  select(property_ID, group) %>% 
  rbind(select(mutate(property_bc, group = "Burnaby"), property_ID, group)) %>%
  st_intersection(select(buffer_CoV_Burnaby, -everything()))

# Active listings within the buffer
daily_buffer <- 
  rbind(select(daily,
               property_ID, date, status, price, listing_type, housing, multi, FREH_3), 
        select(daily_bc,
               property_ID, date, status, price, listing_type, housing, multi, FREH_3)) %>% 
  inner_join(st_drop_geometry(property_buffer))

active_listings <- 
  daily_buffer %>% 
  filter(housing, status != "B") %>% 
  count(date, group) %>% 
  group_by(group) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

# Active listings for the Burnaby-Vancouver buffer, indexed --------------------------------

active_listings_indexed <- 
  active_listings %>%
  filter(date >= key_date_regulations - years(1)) %>%
  group_by(group) %>% 
  mutate(index = 100*n/n[date == key_date_regulations])

figure_3_5_1 <- 
  active_listings_indexed %>% 
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 135, yend = 140, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-06-16"), y = 130,
           label = "COVID-19 \nAirbnb's response", family = "Futura Condensed") +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 130, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations", family = "Futura Condensed")+
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_5_1.pdf", plot = figure_3_5_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5_1.pdf")


# Revenue for CoV and Burnaby ---------------------------------------------------

revenue <-
  daily_buffer %>%
  filter(housing, status == "R") %>%
  group_by(date, group) %>% 
  summarize(revenue = sum(price)) %>% 
  group_by(group) %>% 
  mutate(revenue = slide_dbl(revenue, mean, .before = 6, .complete = TRUE))

revenue_indexed <- 
  revenue %>%
  filter(date >= key_date_regulations - years(1)) %>%
  group_by(group) %>% 
  mutate(index = 100*revenue/revenue[date == key_date_regulations])

figure_3_5_2 <- 
  revenue_indexed %>% 
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 135, yend = 140, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-06-16"), y = 130,
           label = "COVID-19 \nAirbnb's response", family = "Futura Condensed") +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 130, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations", family = "Futura Condensed")+
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_5_2.pdf", plot = figure_3_5_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5_2.pdf")

# Availabilities (R or A) for buffer ---------------------------------------------------

active_listings <- 
  daily_buffer %>% 
  filter(housing, status == c("R","A")) %>% 
  count(date, group, status) %>% 
  group_by(group, status) %>% 
  mutate(n = slide_dbl(n, mean, .before = 13, .complete = TRUE)) %>% 
  ungroup()

active_listings_indexed <- 
  active_listings %>%
  filter(date >= key_date_regulations - years(1)) %>%
  group_by(group, status) %>% 
  mutate(index = 100*n/n[date == key_date_regulations])

figure_3_5_3 <- 
  active_listings_indexed %>% 
  ungroup() %>% 
  mutate(status = ifelse(status == "R", "Reserved", "Available")) %>% 
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 140, yend = 145, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-8-16"), y = 135,
           label = "COVID-19 \nAirbnb's response", family = "Futura Condensed") + 
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 125, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations", family = "Futura Condensed")+
  facet_wrap(~status)+
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank()) #+
  #ggtitle("Availabilities (L) and reservations (A), 14 days rolling window")

ggsave("output/figures/figure_3_5_3.pdf", plot = figure_3_5_3, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5_3.pdf")


# Prices variation after regulations for Burnaby and Vancou ----------------------------

avg_price <-
  daily_buffer %>%
  filter(housing, status == "R") %>%
  group_by(date, group) %>% 
  summarize(avg_price = mean(price)) %>% 
  group_by(group) %>% 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 6, .complete = TRUE))

daily_buffer %>% 
  distinct(property_ID, .keep_all = T) %>% 
  count(group)

avg_price_indexed <- 
  avg_price %>%
  filter(date >= key_date_regulations - years(1)) %>%
  group_by(group) %>%
  mutate(index = 100*avg_price/avg_price[date == key_date_regulations])

figure_3_5_4 <- 
  avg_price_indexed %>% 
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 140, yend = 145, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-8-16"), y = 135,
           label = "COVID-19 \nAirbnb's response", family = "Futura Condensed") +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 125, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations", family = "Futura Condensed")+
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_5_4.pdf", plot = figure_3_5_4, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5_4.pdf")


# Commercial listings buffer, indexed ---------------------------------------------------

commercial_listings_buffer <- 
  daily_buffer %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial, group) %>% 
  group_by(commercial, group) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()

commercial_listings_buffer_indexed <-
  commercial_listings_buffer %>% 
  filter(date >= key_date_regulations - years(1)) %>%
  filter(commercial == TRUE) %>% 
  mutate(index = 100*n/n[date == key_date_regulations])

figure_3_5_5 <- 
  commercial_listings_buffer_indexed %>%
  ggplot(aes(date, index, colour = group)) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2020-07-01"), xend = key_date_covid + days(10),
           y = 140, yend = 145, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2020-06-16"), y = 135,
           label = "COVID-19 \nAirbnb's response", family = "Futura Condensed") +
  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("curve", x = as.Date("2018-12-01"), xend = key_date_regulations + days(10),
           y = 130, yend = 125, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-01"), y = 127,
           label = "Regulations", family = "Futura Condensed") +
  geom_line(lwd = 1)+
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1)])+
  scale_y_continuous(name = NULL) +
  scale_x_date(name = NULL)+
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())

ggsave("output/figures/figure_3_5_5.pdf", plot = figure_3_5_5, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5_5.pdf")


# FREH buffer, indexed ---------------------------------------------------
#
##FREH_buffer_indexed <- 
#  daily_buffer %>% 
#  filter(date >= key_date_regulations - years(1)) %>%
#  group_by(date, group) %>% 
#  summarize(n=sum(FREH_3)) %>% 
#  group_by(group) %>% 
#  mutate(index = 100*n/n[date == key_date_regulations]) 
#
#FREH_buffer_indexed %>%
#  ggplot(aes(date, index, colour = group)) +
#  annotate("segment", x = key_date_covid, xend = key_date_covid,
#           y = -Inf, yend = Inf, alpha = 0.3) +
#  annotate("segment", x = key_date_regulations, xend = key_date_regulations,
#           y = -Inf, yend = Inf, alpha = 0.3) +
#  geom_line(lwd = 1)+
#  scale_colour_manual(name = NULL, values = col_palette[c(3, 2)])+
#  scale_y_continuous(name = NULL) +
#  scale_x_date(name = NULL)+
#  theme_minimal() +
#  theme(legend.position = "bottom", panel.grid.minor.x = element_blank())




burnaby <-
  CMA %>%
  filter(name =="Burnaby (CY)")

buffer <- st_intersection(buffer_CoV_Burnaby, st_union(city, burnaby))

ggplot()+
  geom_sf(data = mutate(city, City = "Vancouver"), aes(fill = City))+
  geom_sf(data = mutate(burnaby, City = "Burnaby"), aes(fill  = City))+
  geom_sf(data = buffer, alpha = 0.7)+
  geom_sf(data = property_buffer, size = 1)+
  coord_sf(
    xlim = c(st_bbox(buffer)["xmin"]-2000, st_bbox(buffer)["xmax"]+2000),
    ylim = c(st_bbox(buffer)["ymin"]-1000, st_bbox(buffer)["ymax"]+1000))+
  scale_fill_manual(values = c("Burnaby" = col_palette[c(1)], "Vancouver" = col_palette[c(5)]))+
  theme_void()
  
  