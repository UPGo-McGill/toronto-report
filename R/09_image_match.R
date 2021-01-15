#### 09 IMAGE MATCH ############################################################

#' This script is extremely time-consuming and memory-intensive to run, so it 
#' should only be rerun when image matching needs to be rebuilt from scratch. In 
#' addition, the script downloads hundreds of thousands of photos to a specified 
#' folder, so it requires approximately 50 GB of free storage space.
#' 
#' Output:
#' - `img_sigs.qsm`
#' - `matches_raw.qsm`
#' - `match_changes.qsm`
#' 
#' Script dependencies:
#' - `07_registration_scrape.R`
#' - `08_ltr_data_import.R`
#' 
#' External dependencies:
#' - Access to the UPGo database
#' - Listings scraped from Kijiji and Craigslist with upgo::upgo_scrape_kj and
#'   upgo::upgo_scrape_cl

source("R/01_startup.R")
library(matchr)
library(furrr)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_raw.qs", nthreads = availableCores())
rm(daily, host, exchange_rates)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data 2/Scrape photos/vancouver"


# Get AB image URLs -------------------------------------------------------

# Get AB urls
ab_urls <- 
  property %>% 
  mutate(urls = if_else(is.na(ab_image_url), ha_image_url, ab_image_url)) %>% 
  pull(urls) %>% 
  str_replace('(?<=(jpg|JPEG|jpeg|JPG)).*', '')

# Get AB IDs
ab_ids <- property$property_ID

# Remove already downloaded images
ab_paths <- 
  list.files(paste0(dl_location, "/ab")) %>% 
  str_remove(".(jpg|jpeg|JPG|JPEG)")

ab_urls <- ab_urls[!ab_ids %in% ab_paths]
ab_ids <- ab_ids[!ab_ids %in% ab_paths]


# Get KJ image urls -------------------------------------------------------

# Get KJ urls
kj_urls <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "kj-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(photos)

# Get KJ IDs
kj_ids <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "kj-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(id)

# Remove already downloaded images
kj_paths <- 
  list.files(paste0(dl_location, "/kj")) %>% 
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>% 
  str_remove("-[:digit:]$")

kj_urls <- kj_urls[!kj_ids %in% kj_paths]
kj_ids <- kj_ids[!kj_ids %in% kj_paths]


# Get CL image urls -------------------------------------------------------

# Get CL urls
cl_urls <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "cl-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(photos)

# Get CL IDs
cl_ids <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "cl-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(id)

# Remove already downloaded images
cl_paths <- 
  list.files(paste0(dl_location, "/cl")) %>% 
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>% 
  str_remove("-[:digit:]$")

cl_urls <- cl_urls[!cl_ids %in% cl_paths]
cl_ids <- cl_ids[!cl_ids %in% cl_paths]

rm(property, ltr)


# Make download subfolders ------------------------------------------------

if (!dir.exists(dl_location)) dir.create(dl_location, 
                                               recursive = TRUE)

if (!dir.exists(paste0(dl_location, "/ab"))) {
  dir.create(paste0(dl_location, "/ab"))
}

if (!dir.exists(paste0(dl_location, "/cl"))) {
  dir.create(paste0(dl_location, "/cl"))
}

if (!dir.exists(paste0(dl_location, "/kj"))) {
  dir.create(paste0(dl_location, "/kj"))
}


# Download images ---------------------------------------------------------

future_map2(ab_urls, ab_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/ab/", .y, ".jpg")))
})

future_map2(cl_urls, cl_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/cl/", .y, "-", seq_along(.x), ".jpg"))) 
})

future_map2(kj_urls, kj_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/kj/", .y, "-", seq_along(.x), ".jpg"))) 
})


# Get new paths -----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location, "/ab"), full.names = TRUE)
cl_paths <- list.files(paste0(dl_location, "/cl"), full.names = TRUE)
kj_paths <- list.files(paste0(dl_location, "/kj"), full.names = TRUE)

rm(dl_location, ab_urls, ab_ids, cl_urls, cl_ids, kj_urls, kj_ids)


# Get signatures ----------------------------------------------------------

ab_sigs <- identify_image(ab_paths, batch_size = 1000)
qsavem(ab_sigs, file = "output/img_sigs.qsm", nthreads = availableCores())

cl_sigs <- identify_image(cl_paths, batch_size = 5000)

cl_sigs_1 <- identify_image(cl_paths[1:100000], batch_size = 5000)
qsavem(ab_sigs, cl_sigs_1, file = "output/img_sigs.qsm", 
       nthreads = availableCores())

cl_sigs_2 <- identify_image(cl_paths[100001:200000], batch_size = 5000)
qsavem(ab_sigs, cl_sigs_1, cl_sigs_2, file = "output/img_sigs.qsm", 
       nthreads = availableCores())

cl_sigs_3 <- identify_image(cl_paths[200001:300000], batch_size = 5000)
qsavem(ab_sigs, cl_sigs_1, cl_sigs_2, cl_sigs_3, file = "output/img_sigs.qsm", 
       nthreads = availableCores())

cl_sigs_4 <- identify_image(cl_paths[300001:400000], batch_size = 5000)
qsavem(ab_sigs, cl_sigs_1, cl_sigs_2, cl_sigs_3, cl_sigs_4, 
       file = "output/img_sigs.qsm", nthreads = availableCores())

cl_sigs_5 <- identify_image(cl_paths[400001:500000], batch_size = 5000)
qsavem(ab_sigs, cl_sigs_1, cl_sigs_2, cl_sigs_3, cl_sigs_4, cl_sigs_5,
       file = "output/img_sigs.qsm", nthreads = availableCores())

cl_sigs_6 <- identify_image(cl_paths[500001:596118], batch_size = 5000)

cl_sigs <- new_matchr_sig_list(
  c(unclass(cl_sigs_1), unclass(cl_sigs_2), unclass(cl_sigs_3),
    unclass(cl_sigs_4), unclass(cl_sigs_5), unclass(cl_sigs_6)))

qsavem(ab_sigs, cl_sigs, file = "output/img_sigs.qsm", 
       threads = availableCores())

kj_sigs <- identify_image(kj_paths, batch_size = 5000)
qsavem(ab_sigs, cl_sigs, kj_sigs, file = "output/img_sigs.qsm", 
       nthreads = availableCores())

rm(ab_paths, cl_paths, kj_paths)


# Get new signatures if necessary -----------------------------------------

ab_sigs_2 <- 
  ab_paths[!ab_paths %in% map_chr(ab_sigs, attr, "file")] %>% 
  identify_image(batch_size = 1000)

ab_sigs <- new_matchr_sig_list(c(unclass(ab_sigs), unclass(ab_sigs_2)))


# Match images ------------------------------------------------------------

ab_matrix <- match_signatures(ab_sigs)
ab_matches <- identify_matches(ab_matrix)
ab_matches <- confirm_matches(ab_matches)
# Temporarily need to remove duplicates!
ab_matches <- ab_matches %>% filter(x_name != y_name)
ab_changes <- compare_images(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)

kj_matrix <- match_signatures(ab_sigs, kj_sigs)
kj_matches <- identify_matches(kj_matrix)
kj_matches <- confirm_matches(kj_matches)
kj_changes <- compare_images(kj_matches)
kj_matches <- integrate_changes(kj_matches, kj_changes)

cl_matrix <- match_signatures(ab_sigs, cl_sigs)
cl_matches <- identify_matches(cl_matrix)
cl_matches <- confirm_matches(cl_matches)
cl_changes <- compare_images(cl_matches)
cl_matches <- integrate_changes(cl_matches, cl_changes)


# Do extra matches for update ---------------------------------------------

ab_matrix_2 <- match_signatures(ab_sigs, ab_sigs_2)
ab_matches_2 <- identify_matches(ab_matrix_2)
ab_matches_2 <- confirm_matches(ab_matches_2)
# Temporarily need to remove duplicates!
ab_matches_2 <- ab_matches_2 %>% filter(x_name != y_name)
ab_changes_2 <- compare_images(ab_matches_2)
ab_matches_2 <- integrate_changes(ab_matches_2, ab_changes_2)

ab_matches <- bind_rows(ab_matches, ab_matches_2)
ab_changes <- bind_rows(ab_changes, ab_changes_2)

kj_matrix_2 <- match_signatures(ab_sigs_2, kj_sigs)
kj_matches_2 <- identify_matches(kj_matrix_2)
kj_matches_2 <- confirm_matches(kj_matches_2)
kj_changes_2 <- compare_images(kj_matches_2)
kj_matches_2 <- integrate_changes(kj_matches_2, kj_changes_2)

kj_matches <- bind_rows(kj_matches, kj_matches_2)
kj_changes <- bind_rows(kj_changes, kj_changes_2)

cl_matrix_2 <- match_signatures(ab_sigs_2, cl_sigs)
cl_matches_2 <- identify_matches(cl_matrix_2)
cl_matches_2 <- confirm_matches(cl_matches_2)
cl_changes_2 <- compare_images(cl_matches_2)
cl_matches_2 <- integrate_changes(cl_matches_2, cl_changes_2)

cl_matches <- bind_rows(cl_matches, cl_matches_2)
cl_changes <- bind_rows(cl_changes, cl_changes_2)


# Save output -------------------------------------------------------------

qsavem(ab_matches, cl_matches, kj_matches, file = "output/matches_raw.qsm",
       nthreads = availableCores())
qsavem(ab_changes, cl_changes, kj_changes, file = "output/match_changes.qsm",
       nthreads = availableCores())
