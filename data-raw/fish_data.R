## # Code to prepare fish_data dataset
##


remotes::install_github("torres-cambas/danubeoccurR")


wkt <- get_wkt(bbox_danube_basin)

# Download occurrences for species of interest
output_occur_path <- "C:/Users/torres/Nextcloud/projects/Danube4all/"
gbif_data <- download_gbif_records(
  species_names = c("Hucho hucho", "Alburnoides bipunctatus", "Chondrostoma nasus"),
  wkt = wkt,
  output_occur_path = output_occur_path,
  gbif_user = ".....",
  gbif_pwd = "......",
  gbif_email = "....@....",
  import_to_r = TRUE
)

# Example dataset cleaning
fish_data <- clean_gbif(gbif_data$raw_download,
                        coordinatePrecision = 0.01,
                        coordinateUncertaintyInMeters = 100,
                        buffer_centroid_countries = 10,
                        buffer_centroid_capitals = 10,
                        buffer_centroid_zoo_herbaria = 10,
                        decimalLongitude = "decimalLongitude",
                        decimalLatitude = "decimalLatitude",
                        speciesKey = "speciesKey",
                        datasetKey = "datasetKey",
                        year = "year",
                        species = "species")

# Save
write.csv(cleaned_df, row.names = FALSE, "./fish_occur.csv")

usethis::use_data(make_fish_data, overwrite = TRUE)
