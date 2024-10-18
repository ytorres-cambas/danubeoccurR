## code to prepare `hydro90m_subcatch_danube` dataset
# Load packages

if (!require(tidyverse)) install.packages("tidyverse", dependencies = T)
#if (!require(sf)) install.packages("sf", dependencies = T)
if (!require(remotes)) install.packages("remotes", dependencies = T)
#if (!require(leaflet)) install.packages("leaflet", dependencies = T)
#if (!require(htmltools)) install.packages("htmltools", dependencies = T)
#if (!require(tmap)) install.packages("tmap", dependencies = T)
remotes::install_github("glowabio/hydrographr")
library(hydrographr)



# Folder to save final Hydrography90m layers
# Create output folder if it doesn't exist
output_hydro90m <- paste0(tempdir(), "/hydrography90m")
if(!dir.exists(output_hydro90m)) dir.create(output_hydro90m)

# Folder to save temporal files
temp <-  paste0(tempdir(), "/temp")
if(!dir.exists(temp)) dir.create(temp)

# Define tile ID
id <- c("h18v02", "h18v04", "h20v02", "h20v04")

# Define raster variable
r_var <- c("sub_catchment")

# raster files
download_tiles(variable = r_var,
               file_format = "tif",
               tile_id = id,
               download_dir = temp)


# Get the full paths of the raster tiles
raster_dir <- list.files(paste0(temp, "/r.watershed"), pattern = ".tif",
                         full.names = TRUE, recursive = TRUE)

# Crop raster tiles to the extent of a vector layer
for(itile in raster_dir) {

  crop_to_extent(raster_layer = itile,
                 bounding_box = bbox_danube_basin,
                 out_dir = temp,
                 file_name =  paste0(str_remove(basename(itile), ".tif"),
                                     "_tmp.tif"))

}

# Merge filtered GeoPackage tiles
merge_tiles(tile_dir = temp,
                                        tile_names = list.files(temp,
                                                                full.names = FALSE,
                                                                pattern = "_tmp.tif"),
                                        out_dir = output_hydro90m,
                                        file_name = "hydro90m_subcatch_danube.tif",
                                        name = "ID",
                                        read = F,
                                        compression = "high",
                                        quiet = FALSE)


hydro90m_subcatch_danube <- terra::rast(paste0(tempdir(), "/hydrography90m/hydro90m_subcatch_danube.tif"))

usethis::use_data(hydro90m_subcatch_danube, overwrite = TRUE)
