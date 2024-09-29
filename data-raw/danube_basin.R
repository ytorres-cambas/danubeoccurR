## code to prepare `danube_basin` dataset goes here

# Load packages

if (!require(tidyverse)) install.packages("tidyverse", dependencies = T)
if (!require(sf)) install.packages("sf", dependencies = T)
if (!require(remotes)) install.packages("remotes", dependencies = T)
remotes::install_github("glowabio/hydrographr")
library(hydrographr)

#path server
#cd /mnt/shared/danube4all

# Folder to save final Hydrography90m layers
# Create output folder if it doesn't exist
output_hydro90m <- paste0(getwd(), "/hydrography90m")
if(!dir.exists(output_hydro90m)) dir.create(output_hydro90m)

# Folder to save temporal files
temp <-  paste0(getwd(), "/temp")
if(!dir.exists(temp)) dir.create(temp)

# Define tile ID
id <- c("h18v02", "h18v04", "h20v02", "h20v04")


# raster files
download_tiles(variable = "basin",
               file_format = "gpkg",
               tile_id = id,
               download_dir = temp)



# Get the full paths of the basin  GeoPackage tiles
basin_dir <- list.files(temp, pattern = "basin_h[v0-8]+.gpkg$",
                        full.names = TRUE, recursive = TRUE)

# Filter basin ID from the GeoPackages of the basin tiles
# Save the filtered tiles
for(itile in basin_dir) {

  filtered_tile <- read_geopackage(itile,
                                   import_as = "sf",
                                   subc_id = 1291835,
                                   name = "ID")


  write_sf(filtered_tile, paste(temp,
                                paste0(str_remove(basename(itile), ".gpkg"),
                                       "_tmp.gpkg"), sep="/"))
}

# Merge filtered GeoPackage tiles
merge_tiles(tile_dir = temp,
            tile_names = list.files(temp, full.names = FALSE,
                                    pattern = "basin_.+_tmp.gpkg$"),
            out_dir = output_hydro90m,
            file_name = "danube_basin.gpkg",
            name = "ID",
            read = FALSE)



usethis::use_data(danube_basin, overwrite = TRUE)
