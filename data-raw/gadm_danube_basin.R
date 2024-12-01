## code to prepare `gadm_danube_basin` dataset goes here

# Load packages

if (!require(tidyverse)) install.packages("tidyverse", dependencies = T)
if (!require(sf)) install.packages("sf", dependencies = T)

gadm_danube_basin <- st_read("./data-raw/gdam_danube_crop.gpkg")

# Create data set
usethis::use_data(gadm_danube_basin, overwrite = TRUE)
