#### 51 CHAPTER 5 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 5. It runs quickly.
#' 
#' Output:
#' - `figure_5_1.pdf`
#' - `figure_5_2.pdf`
#' - `figure_5_3.pdf`
#' - `figure_5_4.pdf`
#' - `figure_5_5.pdf`
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(imager)

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_processed.qs", nthreads = availableCores())
qload("output/matches_raw.qsm", nthreads = availableCores())


# Prepare new objects -----------------------------------------------------

# Distinct LTR listings
ltr_unique <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = TRUE)

# Unique matching property_ID locations
ltr_unique_property_ID <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID) %>% 
  inner_join(unnest(ltr, property_ID), by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = TRUE)


# Figure 5.1 Airbnb/Kijiji image comparison -------------------------------

first_photo_pair <- 
  cl_matches %>% 
  filter(confirmation == "match") %>% 
  filter(x_name == "/Volumes/Data 2/Scrape photos/vancouver/ab/ab-18753643.jpg")

second_photo_pair <- 
  cl_matches %>% 
  filter(confirmation == "match") %>% 
  filter(x_name == "/Volumes/Data 2/Scrape photos/vancouver/ab/ab-10972081.jpg")

titles <- list(
  
  first_photo_pair$x_name %>% 
    str_extract('ab-.*(?=\\.jpg)') %>% 
    {filter(property, property_ID == .)} %>% 
    pull(listing_title),
  
  first_photo_pair$y_name %>% 
    str_extract('cl-.*(?=-[:digit:]\\.jpg)') %>% 
    {filter(ltr, id == .)} %>% 
    slice(1) %>% 
    pull(title) %>% 
    str_remove(' \\|.*'),
  
  second_photo_pair$x_name %>% 
    str_extract('ab-.*(?=\\.jpg)') %>% 
    {filter(property, property_ID == .)} %>% 
    pull(listing_title),
  
  second_photo_pair$y_name %>% 
    str_extract('cl-.*(?=-[:digit:]\\.jpg)') %>% 
    {filter(ltr, id == .)} %>% 
    slice(1) %>% 
    pull(title) %>% 
    str_remove(' - apts.*')
)

photos <- 
  pmap(list(list(first_photo_pair$x_name, first_photo_pair$y_name, 
                 second_photo_pair$x_name, second_photo_pair$y_name), 
            titles,
            c("A", "B", "C", "D")),
       ~{.x %>% 
           load.image() %>% 
           as.data.frame(wide = "c") %>% 
           mutate(rgb = rgb(c.1, c.2, c.3)) %>% 
           ggplot(aes(x, y)) +
           geom_raster(aes(fill = rgb)) + 
           scale_fill_identity() +
           scale_y_continuous(trans = scales::reverse_trans()) +
           ggtitle(paste0(..3, ". ", case_when(
             str_detect(..1, "ab-") ~ "Airbnb",
             str_detect(..1, "cl-") ~ "Craigslist",
             str_detect(..1, "kj-") ~ "Kijiji")),
             subtitle = paste0('"', ..2, '"')) +
           theme_void() +
           theme(plot.title = element_text(family = "Futura",
                                           face = "bold", size = 9),
                 plot.subtitle = element_text(family = "Futura Condensed",
                                              face = "plain", size = 9))})

figure_5_1 <- wrap_plots(photos)

ggsave("output/figures/figure_6_1.pdf", plot = figure_5_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_6_1.pdf")


# Figure 5.2 Date of first LTR listing ------------------------------------

first_ltr_listing <-
  ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj)

figure_5_2 <- 
  first_ltr_listing %>% 
  group_by(kj) %>% 
  mutate(n = slide_dbl(n, mean, .before = 2)) %>% 
  ungroup() %>% 
  filter(created >= "2020-03-01", created <= "2020-09-30") %>% 
  ggplot(aes(created, n, fill = kj)) +
  geom_col(lwd = 0) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_fill_manual(name = NULL, labels = c("Craigslist", "Kijiji"), 
                    values = col_palette[c(1, 3)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain"), #, family = "Futura"
        legend.title = element_text(face = "bold", #, family = "Futura" 
                                    size = 10),
        legend.text = element_text( size = 10)) #, family = "Futura"


ggsave("output/figures/figure_5_2.pdf", plot = figure_5_2, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_2.pdf")


# Figure 5.3 Spatial distribution of listing matches ----------------------

figure_5_3_left <-
  ltr_unique_property_ID %>% 
  select(-geometry) %>% 
  count(area) %>% 
  left_join(LA, .) %>% 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = n), colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3, 4)],
                       limits = c(0, 650),
                       breaks = c(0, 300, 600, 900, 1200, 1500),
                       na.value = "grey80")  +
  guides(fill = guide_colourbar(title = "Total STR to\nLTR matches",
                                title.vjust = 1)) + 
  gg_bbox(LA) +
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(face = "plain",), #family = "Futura"
        legend.title = element_text(face = "bold", #family = "Futura"
                                    size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(size = 5), #family = "Futura"
        panel.border = element_rect(colour = "white", size = 2))

figure_5_3_right <-
  ltr_unique_property_ID %>% 
  select(-geometry) %>% 
  count(area) %>% 
  left_join(count(filter(daily, housing, status != "B", date == "2020-03-01"), 
                  area), by = "area") %>% 
  mutate(pct = n.x / n.y) %>% 
  left_join(LA, .) %>%
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(fill = pct), colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(2, 5)], 
                       na.value = "grey80",
                       limits = c(0.05, .55),
                       breaks = c(0, 0.1, .2, .3, .4, .5),
                       label = scales::label_percent(accuracy = 1))  +
  guides(fill = guide_colourbar(title = "Matches as %\nof active STRs",
                                title.vjust = 1)) + 
  gg_bbox(LA) +
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(face = "plain"), #,family = "Futura" 
        legend.title = element_text(face = "bold", #family = "Futura"
                                    size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(size = 5), #family = "Futura", 
        panel.border = element_rect(colour = "white", size = 2))

figure_5_3 <- figure_5_3_left + figure_5_3_right

ggsave("output/figures/figure_5_3.pdf", plot = figure_5_3, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_3.pdf")


# Figure 5.4 Average asking rents -----------------------------------------

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 6)) %>% 
  ungroup() %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price)) %>% 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 6)) %>% 
  ungroup() %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents) %>% 
  mutate(geography = "City of Vancouver")

asking_rents_dt <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, area == "Downtown") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 6)) %>% 
  ungroup() %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, area == "Downtown") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price)) %>% 
  mutate(avg_price = slide_dbl(avg_price, mean, .before = 6)) %>% 
  ungroup() %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents_dt) %>% 
  mutate(geography = "Downtown") %>% 
  bind_rows(asking_rents)

figure_5_4 <-
  asking_rents %>% 
  filter(created >= "2020-03-13", created <= "2020-09-30") %>% 
  ggplot(aes(created, avg_price, color = status)) +
  geom_line(lwd = 1) +
  annotate("segment", x = key_date_covid, xend = key_date_covid,
           y = -Inf, yend = Inf, alpha = 0.3) +
  annotate("text", x = as.Date("2020-08-15"), y = 3700,
           label = "COVID-19 \nAirbnb's response") + #, family = "Futura Condensed"
  annotate("curve", x = as.Date("2020-08-01"), xend = key_date_covid + days(5),
           y = 3700, yend = 3500, curvature = .2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  scale_x_date(name = NULL, limits = c(as.Date("2020-03-01"), NA)) +
  scale_y_continuous(name = NULL, limits = c(1500,3800), 
                     label = scales::dollar) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1, 3)]) +
  facet_wrap(vars(geography)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(face = "plain"), #, family = "Futura" 
        legend.title = element_text(face = "bold",#family = "Futura", 
          size = 10),
        legend.text = element_text(size = 10), #family = "Futura", 
        strip.text = element_text(face = "bold")) #, family = "Futura"

ggsave("output/figures/figure_5_4.pdf", plot = figure_5_4, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_4.pdf")


# Figure 5.5 STR listing age distributions --------------------------------

first_listing <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(active > "2020-01-01") %>% 
  transmute(property_ID,
            active_length = as.numeric(round((active - created) / 30) / 12),
            matched = if_else(!is.na(ltr_ID), "Matched to LTR", "Not matched"))

figure_5_5 <- 
  first_listing %>% 
  ggplot(aes(active_length, after_stat(width * density), fill = matched)) +
  geom_histogram(bins = 27) +
  scale_x_continuous(name = "Years of activity", limits = c(NA, 10),
                     breaks = c(0:2 * 5)) +
  scale_y_continuous(name = "Percentage of listings",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = NULL, values = col_palette[c(1, 3)]) +
  facet_wrap(vars(matched)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(#family = "Futura", 
          face = "plain"),
        legend.title = element_text(#family = "Futura", 
          face = "bold", 
                                    size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10),
        strip.text = element_text(face = "bold"#, family = "Futura"
                                  ))

ggsave("output/figures/figure_5_5.pdf", plot = figure_5_5, width = 8, 
       height = 2.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_5.pdf")


# Figure 5.6 Host revenue distributions -----------------------------------

annual_revenue <- 
  daily %>%
  filter(housing,
         date <= LTM_end_date, date >= LTM_start_date,
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .) %>%
  st_drop_geometry() %>% 
  mutate(matched = if_else(
    host_ID %in% (filter(property, property_ID %in% 
                           ltr_unique_property_ID$property_ID))$host_ID, 
    "Matched to LTR", "Not matched")) %>%
  group_by(host_ID, matched) %>% 
  summarize(host_rev = sum(revenue_LTM))

figure_5_6 <-
  annual_revenue %>% 
  ggplot(aes(host_rev, after_stat(width * density), fill = matched)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(name = "Annual host revenue", limits = c(0, 100000),
                     labels = scales::dollar_format(scale = 0.001, 
                                                    suffix = "k")) +
  scale_y_continuous(name = "Percentage of hosts", limits = c(NA, .25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = NULL, values = col_palette[c(1, 3)]) +
  facet_wrap(vars(matched)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(face = "plain"), #, family = "Futura" 
        legend.title = element_text(face = "bold",#, family = "Futura" 
                                    size = 10),
        legend.text = element_text(size = 10), #, family = "Futura" 
        strip.text = element_text(face = "bold")) #, family = "Futura"

ggsave("output/figures/figure_5_6.pdf", plot = figure_5_6, width = 8, 
       height = 2.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_6.pdf")


# Figure 5.7 LTR listing age distribution ---------------------------------

length_of_stay <- 
  ltr_unique %>% 
  mutate(active_length = scraped - created) %>% 
  mutate(matched = if_else(!is.na(property_ID), "Matched to STR", 
                           "Not matched"))

figure_5_7 <-  
  length_of_stay %>% 
  ggplot(aes(active_length, after_stat(width * density), fill = matched)) +
  geom_histogram(bins = 27) +
  scale_x_continuous(name = "Days online") +
  scale_y_continuous(name = "Percentage of listings",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = NULL, values = col_palette[c(1, 3)]) +
  facet_wrap(vars(matched)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(face = "plain"), #, family = "Futura"
        legend.title = element_text(face = "bold",#, family = "Futura" 
                                    size = 10),
        legend.text = element_text(size = 10), #, family = "Futura"
        strip.text = element_text(face = "bold")) #, family = "Futura"

ggsave("output/figures/figure_5_7.pdf", plot = figure_5_7, width = 8, 
       height = 2.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_7.pdf")


# Clean up ----------------------------------------------------------------

rm(ab_matches, annual_revenue, asking_rents, asking_rents_vm, boroughs, 
   boroughs_raw, city, cl_matches, DA, figure_5_1, figure_5_2, figure_5_3, 
   figure_5_3_left, figure_5_3_right, figure_5_4, figure_5_5, figure_5_6, 
   figure_5_7, first_listing, first_ltr_listing, first_photo_pair, kj_matches, 
   length_of_stay, ltr, ltr_unique, ltr_unique_property_ID, photos, province, 
   second_photo_pair, streets, streets_downtown, titles)
