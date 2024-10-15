## code to prepare `gbif_data` dataset
Download occurrences from GBIF


# Install the devtools package if you haven't already
install.packages("devtools")

# Install the danubeoccurR package from GitHub
devtools::install_github("ytorres-cambas/danubeoccurR")


species_list <- species_checklist[1:2]

wkt <- get_wkt(bbox_danube_basin)


# Get the path to the temporary directory
temp_path <- tempdir()

# Download occurrences from GBIF
gbif_data <- download_gbif_records(species_names = species_list,
                                     wkt = wkt,
                                     output_occur_path = temp_path,
                                     gbif_user = "yusdiel",
                                     gbif_pwd = "7PcKW2Yr95E2PbW",
                                     gbif_email = "yusdiel.torres@gmail.com",
                                     import_to_r = TRUE)

# Clean the downloaded GBIF records
# gbif_data <- clean_gbif(occurrences$raw_download,
#                                   coordinateUncertaintyInMeters = 1000,
#                                   coordinatePrecision = 0.01,
#                                   buffer_centroid_zoo_herbaria = 1000,
#                                   buffer_centroid_capitals = 1000,
#                                   buffer_centroid_countries = 1000)

usethis::use_data(gbif_data, overwrite = TRUE)
