#### Registration scraping

library(tidyverse)
library(upgo)
library(strr)

upgo_connect()

property <-
  property_remote %>%
  filter(country == "Canada", city == "Toronto") %>%
  collect()

reg_1 <- qs::qread("output/reg_1.qs")
reg_2 <- qs::qread("output/reg_2.qs")

inner_join(reg_0, reg_1)


upgo_scrape_connect(chrome = "87.0.4280.87")

reg_new <-
  property %>%
  filter(!property_ID %in% reg$property_ID) %>%
  slice(1:10000) %>%
  upgo_scrape_ab_registration(.proxy_list, 5)

reg <- bind_rows(reg, reg_new)
qs::qsave(reg, file = "reg.qs")

reg %>%
  # filter(is.na(registration)) #%>%
  filter(registration != "NO LISTING")

upgo_scrape_disconnect()


reg %>%
  filter(!registration %in% c("NO LISTING", "HOMEAWAY")) %>%
  filter(!is.na(registration))

reg %>%
  filter(!registration %in% c("NO LISTING", "HOMEAWAY")) %>%
  filter(!is.na(registration)) %>%
  filter(registration != "Exempt")

live_IDs <-
  reg %>%
  filter(!registration %in% c("NO LISTING", "HOMEAWAY"))

reg_2 <-
  reg %>%
  filter(property_ID %in% live_IDs$property_ID) %>%
  filter(!property_ID %in% reg_2$property_ID) %>%
  upgo_scrape_ab_registration(.proxy_list, 10) %>%
  bind_rows(reg_2)

qs::qsave(reg_2, file = "reg_2.qs")

reg %>% count(registration, sort = TRUE)

reg_2 %>% count(registration, sort = TRUE)

live_IDs <-
  reg_2 %>%
  filter(!registration %in% c("NO LISTING", "HOMEAWAY"))

reg_3 <-
  reg_2 %>%
  filter(property_ID %in% live_IDs$property_ID) %>%
  upgo_scrape_ab_registration(.proxy_list, 10)


