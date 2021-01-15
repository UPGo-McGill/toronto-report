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
LTM_start_date <- as.Date("2019-01-01")
LTM_end_date <- as.Date("2019-12-31")
LTM20_start_date <- as.Date("2020-01-01")
LTM20_end_date <- as.Date("2020-12-31")
col_palette <-
  c("#990033", "#99CC66", "#FF9999", "#333366", "#346900", "#C9C9FF", "#C90A4A")


# Optionally install and activate fonts -----------------------------------

# suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))
#
# read_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb")) %>%
#   mutate(FamilyName = if_else(str_detect(FontName, "Condensed") == TRUE,
#                               "Futura Condensed", FamilyName)) %>%
#   write_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb"))
#
# extrafont::loadfonts()
