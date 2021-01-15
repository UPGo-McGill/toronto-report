#### 50 CHAPTER 5 ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 5. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_processed.qs", nthreads = availableCores())
qload("output/cmhc.qsm", nthreads = availableCores())


# Prepare new objects -----------------------------------------------------

# distinct LTR listings
ltr_unique <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = TRUE)

# Unique STR matches with their information on longer-term market
ltr_unique_property_ID <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID) %>% 
  inner_join(unnest(filter(ltr, !is.na(property_ID)), property_ID), 
             by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = T)

# Possible private rooms or housing swaps in the LTR market scrape
ltr_PR <- 
  ltr_unique %>% 
  mutate(title = tolower(title)) %>% 
  filter(str_detect(title, paste0("(^| |/)(room|chambre|swap|exchange|1 ",
                                  "bedroom for rent|1 bdrm|1 fully furnished ",
                                  "bedroom|échange|lease transfer|spacious ","
                                  bedroom)( |$|/)")))


# How many STR listings have returned to the long-term market? ------------

#' Our image matching algorithm recognized 1,290 [1] unique Airbnb listings 
#' which matched with 2,537 [2] different LTR listings (as some units are posted 
#' multiple times) in the City of Vancouver. The matching LTR listings were 
#' emajorily posted on Craigslist (92.4%), with only 7.6% on Kijiji.
#' 51.2% (1,294 [3] listings) were active STRs in 2020, which establishes a 
#' lower bound for the number of unique housing units that went from the STR 
#' market to the LTR market due to the COVID-19 pandemic.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow() %>% prettyNum(",")

#' [2] Unique LTR matches
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  nrow() %>% 
  prettyNum(",")

#' [3] KJ/CL split
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(kj) %>% 
  mutate(pct = n/sum(n))

#' [4] Unique STR matches active in 2020
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(active = active >= "2020-01-01" | created >= "2020-01-01") %>% 
  mutate(pct = n / sum(n))

#' Out of the 112 [1] Airbnb listings which we matched to Kijiji, 65.2% [2]
#' were identified by their hosts as “long-term rentals” and 34.8% [2] were 
#' identified as “short-term rentals”. Among these listings, 33.9% [3] specified 
#' lease lengths of one year, 29.5% [3] specified month-to-month, and 36.6% [3]
#' did not specify. It is important, however, to mention that Kijiji listings
#' represent a marginal fraction of matches.

#' [1] KJ or CL
ltr_unique_property_ID %>% 
  group_by(kj) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

#' [2] Long-term or short-term
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(short_long) %>% 
  mutate(perc = n / sum(n))

#' [3] Agreement length
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(type) %>% 
  mutate(perc = n / sum(n))


# When did STR listings move to the long-term market? ---------------------

#' By the end of the March, more than 62 [1] listings were being transferred 
#' each day. Daily numbers remained high through April, but even from May 
#' through July an average of 6.5 [2] new Airbnb listings were transferred to 
#' Craigslist or Kijiji each day.

#' [1] Peak daily listings transfer
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, sort = TRUE) %>%
  slice(1) %>% 
  pull(n) %>% 
  prettyNum(",")

ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  filter(created >= "2020-03-22", created <= "2020-03-31") %>% 
  count(created, sort = TRUE) %>% 
  slice(1) %>% 
  pull(n) %>% 
  prettyNum(",")

#' [2] Average daily listings transfer, May - August
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj) %>% 
  filter(created >= "2020-05-01", created <= "2020-09-30") %>% 
  summarize(avg = round(mean(n), 1))
  

# Spatial distribution of matched listings --------------------------------

#' Out of the 1,380 [1] unique STR listings matched to LTR listings in the City 
#' of Vancouver, nearly  (% [2]) were located in the Downtown area 
#' and [2] in the XXXX area, followed by XXXX (% [2]), 
#' XXXX (% [2]) and XXXX
#' (% [2]) accounting for most of the remaining matches.... In fact, 
#' the number of STR listings matched to LTR listings in Downtown is 
#' equivalent to nearly half (41.4% [3]) of all the STR listings active in the 
#' borough on March 1, 2020, and 24.7% [4] of all the listings active in the 
#' borough in 2020.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] Total number of unique matches by area
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(area) %>% 
  mutate(pct = round(n / sum(n), 3)) %>% 
  arrange(-pct) %>% 
  slice(1:6)

#' [3] Downtown active March 1 percentage
{{property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(area) %>% 
  mutate(pct = n / sum(n)) %>% 
  slice(2) %>% 
  pull(n)} /
  {daily %>% 
      filter(housing, area == "Downtown", status != "B", 
             date == "2020-03-01") %>% 
      nrow}} %>% 
  scales::percent(0.1)

#' [4] Downtown active 2020 percentage
{{property %>% 
    st_drop_geometry() %>% 
    filter(!is.na(ltr_ID)) %>% 
    count(area) %>% 
    mutate(pct = n / sum(n)) %>% 
    slice(2) %>% 
    pull(n)} /
  {daily %>% 
      filter(housing, area == "Downtown", status != "B", 
             date >= "2020-01-01") %>% 
      pull(property_ID) %>% 
      unique() %>% 
      length()}} %>% 
  scales::percent(0.1)


# Asking rents ------------------------------------------------------------

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
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
  ungroup() %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents_dt) %>% 
  mutate(geography = "Downtown") %>% 
  bind_rows(asking_rents)

#' On March 14 [1], when the average asking rent on LTR platforms in the City 
#' of Vancouver was $1,984 [1], the average asking rent among listings which we 
#' matched to Airbnb was $2,500 [1]—26.0% [1] higher. Over the course of
#' March, average asking rents for LTR listings matched to Airbnb declined 
#' slightly, only to pick up until July, with an average of of $2,726 [2]
#' for the months of April to July. It remained a significantly higher (25.5% [2]) 
#' average asking price than the non-matched listings, which from 
#' April through July averaged $2,172 [2]. From August to mid-October, average rents
#' decreased for both STR matches and non-matches, with matches averaging $2,433 [3]
#' and non-matches, $1,863 [3] - however, the difference between both was larger, with
#' 30.6% [3] difference.

#' [1] Peak difference between matched and city-wide
asking_rents %>% 
  filter(geography == "City of Vancouver", created >= "2020-03-13",
         created <= "2020-04-30") %>% 
  group_by(created) %>% 
  summarize(city = avg_price[status == "All listings"],
            match = avg_price[status == "Matched to STR"],
            dif = avg_price[status == "Matched to STR"] - 
              avg_price[status == "All listings"],
            dif_pct = match / city - 1, .groups = "drop") %>% 
  filter(dif == max(dif)) 

# July 31 values
asking_rents %>% 
  filter(geography == "City of Vancouver", created == "2020-07-31")

#' [2] April-July averages
asking_rents %>% 
  filter(geography == "City of Vancouver", created >= "2020-04-01",
         created <= "2020-07-31") %>% 
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  mutate(pct_higher = avg_price / min(avg_price) - 1)


#' Even in the Downtown area, LTR listings matched to Airbnb have been on 
#' average 7.6% [1] higher than listings not matched, with a pattern of
#' increasing rents until the end of June, yielding to lower (although still high)
#' rents from July to October.

#' [1] Downtown overall average rent differences
asking_rents %>% 
  filter(geography == "Downtown", created >= "2020-03-13", 
         created <= "2020-07-31", status != "All listings") %>% 
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(dif = max(avg_price) / min(avg_price) - 1) %>% 
  pull(dif) %>% 
  scales::percent(0.1)

#' The average city-wide asking rent on Craigslist and Kijiji has remained 
#' between $1,527 [1] and $2,451 [1] throughout the March-mid-October period we tracked 
#' it.... The daily average asking rent in March was $2,802 [2], while in September 
#' it was $2,400 [2]—a $403 [2] or 16.8% [2] decline.

asking_rents %>% 
  filter(created >= "2020-03-13", created <= "2020-10-06", 
         status == "All listings", geography == "City of Vancouver") %>% 
  summarize(min = min(avg_price), max = max(avg_price))

# Rent decrease
asking_rents %>% 
  filter(geography == "City of Vancouver" & status == "All listings" &
           ((created >= "2020-03-25" & created <= "2020-03-31") | 
           (created >= "2020-07-25" & created <= "2020-07-31"))) %>% 
  group_by(july = created >= "2020-07-25") %>% 
  summarize(avg = mean(avg_price)) %>% 
  summarize(dif = avg[1]/avg[2] - 1) %>% 
  pull(dif) %>% 
  scales::percent(0.1)

asking_rents %>% 
  filter(geography == "City of Vancouver", created >= "2020-03-25", 
         created <= "2020-03-31", status == "All listings") %>% 
  summarize(avg = mean(avg_price)) %>% 
  pull(avg) %>% 
  scales::dollar(1)

asking_rents %>% 
  filter(geography == "City of Vancouver", created >= "2020-07-25", 
         created <= "2020-07-31", status == "All listings") %>% 
  summarize(avg = mean(avg_price)) %>% 
  pull(avg) %>% 
  scales::dollar(1)

#' [2] Downtown March/September rent differences
asking_rents %>% 
  filter(geography == "Downtown", created >= "2020-03-13",
         created <= "2020-09-30", status == "All listings") %>% 
  filter(created <= "2020-03-31" | created >= "2020-09-01") %>% 
  group_by(created <= "2020-03-31") %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  mutate(dif = avg_price - min(avg_price),
         dif_pct = round(avg_price / min(avg_price) - 1, 3))


# Listing amenities -------------------------------------------------------

#' Studios were overrepresented among LTR listings which matched to Airbnb 
#' (8.1% [1]) compared with LTR listings which did not match (3.9% [2]), 
#' STR listings (in 2019, 5.1% [3]) and Vancouver’s rental stock (X.X% [4]). 
#' Units with one bedrooms were underrepresented in LTR listings, 
#' at 40.6% [1] for matches and 38.0% [2] for non-matches, compared to the 
#' the STR market (56.6% [3]). Two-bedrooms, which were 
#' considerably underrepresented in STR listings (25.0% [3]) compared to the 
#' overall rental housing stock (XX,X% [4]), constituted 39.2% [1] of LTR 
#' listings that matched with STR listings, and a similar proportion among 
#' non-matched listings (41.0% [2]).

#' [1] Bedrooms in LTR matches
ltr_unique_property_ID %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [2] Bedrooms in LTR non-matches
ltr %>% 
  st_drop_geometry() %>% 
  filter(is.na(property_ID)) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [3] Bedrooms in 2019 STRs
property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2019-01-01", created <= "2019-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [4] Bedrooms in City of Vancouver rentals
city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = round(units / sum(units), 3))

#' Table 5.1 
bedroom_match_table <-
  city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = round(units / sum(units), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  select(bedrooms, pct) %>% 
  mutate(Category = "City of Vancouver rental stock", .before = bedrooms)

bedroom_match_table <-
  property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2019-01-01", created <= "2019-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "STR market (2019)", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <-
  ltr_unique_property_ID %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "LTRs matched to STR", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(is.na(property_ID)) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "LTRs not matched", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <- 
  bedroom_match_table %>% 
  pivot_wider(values_from = pct, names_from = bedrooms)

bedroom_match_table %>% 
  gt() %>% 
  tab_header(title = "STR and LTR listings by bedroom count") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(2:5), decimals = 1)

#' Of the 61,393 [1] LTR listings we analyzed, 25.0% [2] were listed as 
#' furnished, 74.8% [2] as unfurnished, and 0.2% [2] did not provide this 
#' information. Listings which matched with STRs had a significantly higher 
#' proportion classified as furnished: 75.0% [3] furnished and 24.9% [3] 
#' unfurnished, with only 0.1% [3] not providing this information.

#' [1] All scraped LTR listings
ltr_unique %>% nrow()

#' [2] Furnished proportion for all LTRs
ltr_unique %>% 
  count(furnished) %>% 
  mutate(pct = round(n / sum(n), 3))

#' [3] Furnished proportion for matches
ltr_unique_property_ID %>% 
  count(furnished) %>% 
  mutate(pct = round(n / sum(n), 3))


# Are matched listings commercial operations? -----------------------------

#' Of the 1,380 [1] unique STR listings that matched with the LTR market, 
#' 1,127 [2] (83.9% [2]) are entire-home listings and 207 [2] (15.4% [2]) are 
#' private-room listings. Examining the entire-home listings, 52.9% [3] of them 
#' were identified as frequently rented entire-home (FREH) listings at some 
#' point, which means they were almost certainly operated commercially. 
#' Moreover, 54.1% [3] of entire-home STR listings which matched to LTR listings 
#' were multilistings at some point, which means they were operated by hosts 
#' controlling multiple listings simultaneously. In total, 74.8% [3] of 
#' entire-home listings had one of these two strong indicators of commercial 
#' activity.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] listing_type breakdown among matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  mutate(pct = round(n / sum(n), 3))

#' [3] Commercial status among EH matches
daily %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID,
         listing_type == "Entire home/apt") %>% 
  group_by(property_ID) %>% 
  summarize(FREH = as.logical(sum(FREH_3 > 0.5)),
            ML = as.logical(sum(multi))) %>% 
  summarize(total = 
              property %>% 
              st_drop_geometry() %>% 
              filter(property_ID %in% ltr_unique_property_ID$property_ID, 
                     listing_type == "Entire home/apt") %>% 
              nrow(),
            FREH_pct = sum(FREH) / total,
            ML_pct = sum(ML) / total,
            either_pct = sum(ceiling((FREH + ML) / 2)) / total)

#' The 207 [1] private-room listings require some further analysis, because each 
#' of these listings matched to a Craigslist or Kijiji listing advertised as an 
#' entire housing unit. Our analysis suggests that these listings break down 
#' into three categories. The first is miscategorizations. 7 [2] (21.9% [2]) of 
#' the LTR listings that matched to STR private-room listings had titles such as 
#' “1 fully furnished bedroom” or “swap”.

#' [1] PR matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  mutate(pct = n / sum(n)) %>% 
  filter(listing_type == "Private room")

#' [2] Miscategorizations
property %>% 
  filter(listing_type == "Private room",
         property_ID %in% unlist(ltr_PR$property_ID)) %>% 
  nrow() %>% 
  {c(., . / {property %>% 
      st_drop_geometry() %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      count(listing_type) %>% 
      mutate(pct = n / sum(n)) %>% 
      filter(listing_type == "Private room") %>% 
      pull(n)})} %>% 
  round(3)

#' 108 [1] (52.2%) [1] of the 207 [2] private-room listings which matched to 
#' Craiglist or Kijiji were listings identified as belonging to ghost hostels in 
#' Vancouver. 

#' [1] GH matches
property %>% 
  filter(listing_type == "Private room") %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  filter(property_ID %in% unlist(GH$property_IDs)) %>% 
  nrow() %>% 
  {c(., . / {property %>% 
      st_drop_geometry() %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      count(listing_type) %>% 
      filter(listing_type == "Private room") %>% 
      pull(n)})} %>% 
  round(3)

#' [2] PR matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  filter(listing_type == "Private room") %>% 
  pull(n)

#' The remaining 92 [1] private-room Airbnb listings which matched to 
#' Craigslist or Kijiji are likely to be ghost hostels which our algorithms 
#' failed to identify, or smaller housing units similarly subdivided into 
#' private rooms.

#' [1] Leftover PR matches
{property %>% 
    st_drop_geometry() %>% 
    filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
    count(listing_type) %>% 
    filter(listing_type == "Private room") %>% 
    pull(n)} - {property %>% 
  filter(listing_type == "Private room",
         property_ID %in% unlist(ltr_PR$property_ID)) %>% 
  nrow()} - {property %>% 
      filter(listing_type == "Private room") %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      filter(property_ID %in% unlist(GH$property_IDs)) %>% 
      nrow()}

#' Focusing on the unambiguous case of the entire-home listings which matched 
#' between STR and LTR platforms, 27.9% [1] of the commercial listings active 
#' in 2020 have been transferred to Craiglist or Kijiji since March.... 
#' Expressed as a percentage of the commercial listings active on March 1, 2020, 
#' at the onset of the pandemic, the matches represent 48.0% [2] of these 
#' listings.

#' [1] EH matches as % of commercial operations on March 1
{property %>% 
    filter(property_ID %in% ltr_unique_property_ID$property_ID, 
           listing_type == "Entire home/apt") %>% 
    nrow() /
    daily %>% 
    filter(listing_type == "Entire home/apt", status != "B", 
           date >= "2020-01-01", (FREH_3 > 0.5 | multi == TRUE)) %>% 
    count(property_ID) %>% 
    nrow()} %>% 
  round(3)

#' [2] EH matches as % of commercial operations on March 1
{property %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID, 
         listing_type == "Entire home/apt") %>% 
  nrow() /
  daily %>% 
  filter(listing_type == "Entire home/apt", status != "B", date == "2020-03-01", 
         (FREH_3 > 0.5 | multi == TRUE)) %>% 
  nrow()} %>% 
  round(3)


# Which STR hosts transferred their listings to Craigslist and Kij --------

#' In Vancouver, XXXX [1] unique Airbnb host IDs were linked to the XXXX [2] 
#' LTR matches. XXX [3] of these hosts posted more than one of their STR units 
#' on Craigslist or Kijiji. For example, the top host network in Montreal 
#' (discussed in section 2) posted 239 [3] of its STR units on the LTR market. 
#' Half (49.7% [4]) of the active properties of these 1,149 [1] hosts were found
#'  on either Kijiji, Craigslist, or both. 

#' [1] number of hosts found on a LTR platform
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), !is.na(host_ID)) %>% 
  count(host_ID, sort = TRUE) %>% 
  nrow() 

#' [2] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [3] Number of hosts with > 1 property on LTR market
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), !is.na(host_ID)) %>% 
  count(host_ID, sort = TRUE) %>% 
  filter(n > 1) %>%
  summarize(total = n()) %>% 
  pull(total) %>% 
  prettyNum(",")

#' [4] Percentage of hosts' properties which made the switch from STR to LTR
{nrow(ltr_unique_property_ID) / 
    property %>% 
    st_drop_geometry() %>% 
    filter(host_ID %in% (filter(property, !is.na(ltr_ID)))$host_ID, 
           scraped >= "2020-01-01") %>% 
    nrow()} %>% 
  scales::percent(0.1)

#' The median STR host revenue was $8,700 [1] in the entire City of Vancouver in 
#' 2019. The annual median revenue of hosts who transferred listings to the LTR 
#' market was $ [2], while the median revenue of hosts who did not 
#' transfer listings was only $4,000 [2]. Moreover, many of Vancouver’s highest 
#' earning STR hosts turned to LTR platforms during the COVID-19 pandemic. For 
#' example, 3 [3] of the 5 [3] hosts that made more than $500,000 in the past 
#' year listed at least one of their STR units on an LTR platform. On average 
#' these top earning hosts listed 2 [4] units on LTR platforms, compared to 
#'  units [4] for all other hosts). 

revenue_2019 <- 
  daily %>%
  filter(housing,
         date <= LTM_end_date, date >= LTM_start_date,
         status == "R", !is.na(host_ID)) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .) %>% 
  st_drop_geometry() %>% 
  select(property_ID, host_ID, listing_type:active, revenue_LTM)

quarter_mil_ltr <- 
  revenue_2019 %>% 
  group_by(host_ID) %>% 
  summarize(host_rev = sum(revenue_LTM)) %>% 
  filter(host_rev > 250000)

#' [1] Median revenue
revenue_2019$revenue_LTM %>% 
  median() %>% 
  round(-2) %>% 
  scales::dollar()

#' [2] Median host revenue by host match status
revenue_2019 %>% 
  group_by(host_match = host_ID %in% (filter(property, 
                                             !is.na(ltr_ID)))$host_ID,
           host_ID) %>%
  summarize(host_rev = sum(revenue_LTM)) %>% 
  summarize("median_rev" = scales::dollar(round(median(host_rev), -2)))

#' [3] Hosts that matched and made more than 250k
quarter_mil_ltr %>% 
  summarize(
    total = n(),
    host_match = sum(
      host_ID %in% (filter(property, property_ID %in% 
                             ltr_unique_property_ID$property_ID)$host_ID)))

#' [4] Average number of listings by host match status
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>%
  count(host_ID) %>% 
  group_by(host_match = host_ID %in% quarter_mil_ltr$host_ID) %>% 
  summarize(avg = round(mean(n), 1))

#' Out of all hosts with active STR listings in 2020 that shifted their listings 
#' to the LTR market in Vancouver, XX.X% [1] had Superhost status. This is almost 
#' twice [1] as high as the general XX.X% [1] prevalence of Superhost status 
#' among Vancouver hosts, which is consistent with the idea that the listings 
#' moving from STR to LTR platforms in Vancouver are dominated by commercial 
#' operations. 

#' [1] Superhost status
property %>%
  st_drop_geometry() %>%
  filter(!is.na(host_ID), !is.na(superhost), scraped >= "2020-01-01") %>% 
  group_by(match = !is.na(ltr_ID), superhost) %>% 
  count(host_ID) %>% 
  group_by(match, host_ID) %>% 
  summarize(superhost = as.logical(sum(superhost))) %>% 
  summarize(pct = sum(superhost) / n()) %>% 
  mutate(dif = pct[2] / pct[1])


# Are matched listings successfully rented, or still active on Air --------

#' STR matches were listed an average of 23.0 [1] days on LTR platforms, while 
#' non-matches were listed only three quarters as long—17.1 [1] days on average.

#' [1] length of availability on LTR platforms
ltr_unique %>% 
  group_by(matched = !is.na(property_ID)) %>% 
  summarize(time =round(mean(scraped - created, na.rm = TRUE), 1))

#' Out of the total 1,380 [1] Airbnb listings which we identified on LTR 
#' platforms, 1,475 [2] (58.4% [2]) were still present on Airbnb at the
#' beginning of 2020. Out of this number, 916 [2] (62.1% [2]) were still present
#' by the end of July 2020 (the last day for which we have data), while the 
#' other 559 [2] (37.9% [2]) had been deactivated. Extrapolating this proportion 
#' across the entire set of matched listings we identified, we estimate that 
#' 957 [2] matched listings have been deactivated from Airbnb during the
#' pandemic, while 1,569 [2] remain on the platform.... However, 75.6% [3] of 
#' these listings were rented as furnished rentals on Craigslist or Kijiji. 

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] Number of matches still listed on Airbnb
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(total = n(),
            total_pct = total / nrow(filter(property, !is.na(ltr_ID))),
            n_scraped = sum(scraped >= "2020-09-30"),
            pct_scraped = mean(scraped >= "2020-09-30"),
            not_scraped = total - n_scraped,
            pct_not_scraped = 1 - pct_scraped,
            n_gone = pct_not_scraped * nrow(filter(property, !is.na(ltr_ID))),
            n_active = pct_scraped * nrow(filter(property, !is.na(ltr_ID))))

#' [3] Furnished rentals with deactivated Airbnb listings
property %>% 
  st_drop_geometry() %>% 
  filter( !is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01", scraped < "2020-09-30") %>% 
  left_join(select(ltr_unique_property_ID, property_ID, furnished)) %>% 
  filter(!is.na(furnished)) %>% 
  summarize(furnished = scales::percent(mean(furnished), 0.1)) %>% 
  pull(furnished)

#'  Of the 916 [1] matched listings which were present on Airbnb at the 
#'  beginning of 2020 and still present by the end of July, 393 [1] (42.9% [1]) 
#'  were blocked for the entirety of the month of July.

#' [1] Inactive in September
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-09-30") %>% 
  summarize(total = n(),
            n_inactive = total - sum(active >= "2020-09-01", na.rm = TRUE) +
              sum(is.na(active)),
            pct_inactive = n_inactive / total)
  
#' In total, taking into account the matched listings which have continued to 
#' see activity on Airbnb, we estimate that, of the total 2,526 [1] STR listings 
#' which were advertised on Craigslist or Kijiji, 957 [1] (37.9% [1]) have been 
#' permanently deactivated from Airbnb and have likely transitioned back to 
#' long-term housing, 615 [1] (24.3% [1]) have been temporarily blocked on 
#' Airbnb and have likely been rented in the long-term market but may return to 
#' being STRs in the future, and 954 [1] (37.8% [1]) failed to be rented on 
#' LTR platforms and instead remain active on Airbnb.

#' [1] Total active/deactivated/inactive estimates
deactivated_pct <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(pct_not_scraped = 1 - mean(scraped >= "2020-09-01")) %>% 
  pull(pct_not_scraped)

blocked_pct <-
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(pct_blocked = (sum(active < "2020-09-01" & scraped >= "2020-09-01", 
                               na.rm = TRUE) + sum(is.na(active) & scraped >= 
                                                     "2020-09-01")) / n()) %>% 
  pull(pct_blocked)

total_table <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  summarize(total = n(),
            deactivated = total * deactivated_pct,
            blocked = total * blocked_pct,
            active = total - deactivated - blocked) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  pivot_longer(-total) %>% 
  mutate(pct = round(value / sum(value), 3))


# Clean up ----------------------------------------------------------------

rm(annual_avg_rent, annual_units, annual_vacancy, asking_rents, asking_rents_vm,
   bedroom_match_table, boroughs, boroughs_raw, city, city_avg_rent, city_units,
   city_vacancy, cmhc, DA, half_mil_ltr, ltr, ltr_PR, ltr_unique,
   ltr_unique_property_ID, province, revenue_2019, streets, streets_downtown,
   blocked_pct, deactivated_pct)
