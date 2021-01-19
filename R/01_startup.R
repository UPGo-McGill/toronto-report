#### 01 STARTUP ################################################################

# There is usually no need to run this script directly; it is sourced from the
# other scripts which need it.


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)
library(future)
library(progressr)
library(slider)
library(data.table)
library(gt)
library(extrafont)
library(patchwork)
library(qs)


# Set global variables ----------------------------------------------------

if (Sys.info()["sysname"] != "Windows") plan(multisession)
key_date_covid <- as.Date("2020-03-14")
key_date_regulations <- as.Date("2020-12-31")
start_2019 <- as.Date("2019-01-01")
end_2019 <- as.Date("2019-12-31")
start_2020 <- as.Date("2020-01-01")
end_2020 <- as.Date("2020-12-31")
col_palette <-
  c("#14325C", "#5398D9", "#F4E3B1", "#D96B0C", "#A53A3B", "#A2E8AF")


# Optionally install and activate fonts -----------------------------------

# suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))
#
# read_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb")) %>%
#   mutate(FamilyName = if_else(str_detect(FontName, "Condensed") == TRUE,
#                               "Futura Condensed", FamilyName)) %>%
#   write_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb"))
#
# extrafont::loadfonts()
