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
reg_3 <- qs::qread("output/reg_3.qs")

upgo_scrape_connect(chrome = "87.0.4280.87")


upgo_scrape_disconnect()


live_IDs <-
  reg_3 %>%
  filter(!registration %in% c("NO LISTING", "HOMEAWAY")) %>%
  pull(property_ID) %>%
  c({property %>%
      filter(!property_ID %in% reg_1$property_ID) %>%
      pull(property_ID)})


reg_4 <- reg_3[0,]
reg_4 <- qs::qread("output/reg_4.qs")


while ({property %>%
    filter(property_ID %in% live_IDs) %>%
    filter(!property_ID %in% reg_4$property_ID) %>%
    nrow()} > 0) {

  reg_4 <-
    property %>%
    filter(property_ID %in% live_IDs) %>%
    filter(!property_ID %in% reg_4$property_ID) %>%
    slice(1:5000) %>%
    upgo_scrape_ab_registration(.proxy_list, 10) %>%
    bind_rows(reg_4)

  qs::qsave(reg_4, file = "output/reg_4.qs")

}

