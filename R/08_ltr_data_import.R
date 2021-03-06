#### 07 LTR DATA IMPORT ########################################################

#' This script should only be rerun when new KJ/CL data arrives or it otherwise
#' needs to be rebuilt from scratch.
#'
#' Output:
#' - `ltr_raw.qs`
#'
#' Script dependencies:
#' - None
#'
#' External dependencies:
#' - `kj.qs` and `cl.qs`, the results of scraping Kjiji and Craigslist
#'   listings, respectively.

source("R/01_startup.R")


# Load and filter data ----------------------------------------------------

kj <- qread("data/ltr/kj.qs", nthreads = availableCores()) %>%
  filter(city == "Toronto")

cl <- qread("data/ltr/cl.qs", nthreads = availableCores()) %>%
  filter(city == "toronto")


# Clean location field ----------------------------------------------------

kj <-
  kj %>%
  mutate(location = str_remove(location, "^([:punct:]|[:space:])*"))


# Get geometry from KJ listings -------------------------------------------

# Find previously geocoded addresses
# upgo_connect(geolocation = TRUE)

# Temp workaround
geolocation_remote <-
  read_csv("data/geo.csv") %>%
  set_names(c("entity", "lon", "lat"))

processed_addresses <-
  geolocation_remote %>%
  filter(entity %in% !!kj$location) %>%
  collect()

upgo_disconnect()

kj_old_geography <-
  kj %>%
  inner_join(processed_addresses, by = c("location" = "entity"))

kj_new_geography <-
  kj %>%
  filter(!location %in% processed_addresses$entity, !is.na(location))

if (nrow(kj_new_geography) > 0) {

  library(ggmap)
  to_geocode <- kj_new_geography$location %>% unique()
  output <- ggmap::geocode(to_geocode)
  output <- tibble(location = to_geocode, lon = output$lon, lat = output$lat)

  kj_new_geography <-
    kj_new_geography %>%
    left_join(output, by = "location")

} else kj_new_geography <- mutate(kj_new_geography, lon = numeric(),
                                  lat = numeric())

locations_new <-
  kj_new_geography %>%
  select(entity = location, lon, lat)

locations_new <-
  locations_new %>%
  distinct(entity, .keep_all = TRUE)

# Upload new geocoding results to server (ONLY WORKS WITH ADMIN PRIVILEGES)
upgo_connect(geolocation = TRUE)
RPostgres::dbWriteTable(upgo:::.upgo_env$con, "geolocation", locations_new,
                        append = TRUE)

upgo_disconnect()

# TEMP WORKAROUND
geolocation_remote <-
  geolocation_remote %>%
  bind_rows(locations_new)

write_csv(geolocation_remote, "data/geo.csv")

# Rbind results
kj <- bind_rows(kj_old_geography, kj_new_geography)

suppressWarnings(rm(processed_addresses, kj_old_geography, kj_new_geography,
   locations_new, output, to_geocode))


# Clean up KJ file --------------------------------------------------------

kj <-
  kj %>%
  mutate(bedrooms = str_replace(bedrooms, pattern = "Bachelor/Studio|Studio",
                                replacement = "0"),
         bedrooms = as.numeric(str_sub(bedrooms, end = 1L)),
         bathrooms = str_sub(bathrooms, end = 3L),
         bathrooms = str_replace(bathrooms, pattern = "Ut|U|\\+",
                                 replacement = ""),
         bathrooms = as.numeric(bathrooms),
         type = details %>%
           str_extract("(?<=Agreement Type).*?(?=(Move-In)|(Pet Friendly))") %>%
           str_remove('</dd.*') %>%
           str_remove('.*">'),
         type = if_else(type == "Not Available", NA_character_, type))

kj <-
  kj %>%
  select(id, short_long:furnished, type, lat, lon,
         title, text, photos) %>%
  mutate(kj = TRUE)

kj_with_geom <-
  kj %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

kj_without_geom <-
  kj %>%
  filter(is.na(lon) | is.na(lat)) %>%
  mutate(geometry = st_sfc(st_point())) %>%
  st_as_sf(crs = 4326) %>%
  select(-lon, -lat)

kj <-
  rbind(kj_with_geom, kj_without_geom) %>%
  st_as_sf() %>%
  arrange(scraped, id)

rm(kj_with_geom, kj_without_geom)


# Clean up CL file --------------------------------------------------------

cl <-
  cl %>%
  select(id, created:furnished, title, text, photos) %>%
  separate(location, c("lat", "lon"), sep = ";") %>%
  mutate(city = "Toronto",
         bedrooms = as.numeric(bedrooms),
         bathrooms = as.numeric(bathrooms),
         created = as.Date(created),
         scraped = as.Date(scraped),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         short_long = NA,
         location = NA,
         type = NA,
         kj = FALSE)

cl_with_geom <-
  cl %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cl_without_geom <-
  cl %>%
  filter(is.na(lon) | is.na(lat)) %>%
  mutate(geometry = st_sfc(st_point())) %>%
  st_as_sf(crs = 4326) %>%
  select(-lon, -lat)

cl <-
  rbind(cl_with_geom, cl_without_geom) %>%
  st_as_sf() %>%
  arrange(scraped, id)

rm(cl_with_geom, cl_without_geom)


# Rbind into one table ----------------------------------------------------

ltr <- rbind(kj, select(cl, names(kj)))

rm(kj, cl)


# Add geometry ------------------------------------------------------------

qload("output/geometry.qsm", nthreads = availableCores())

ltr <- st_transform(ltr, 32617)

ltr <-
  ltr %>%
  st_join(WD) %>%
  select(-dwellings)

ltr <-
  ltr %>%
  st_join(DA) %>%
  select(-dwellings) %>%
  as_tibble() %>%
  st_as_sf()

rm(city, CMA, DA, province, WD)


# Save output -------------------------------------------------------------

qsave(ltr, file = "output/ltr_raw.qs", nthreads = availableCores())
