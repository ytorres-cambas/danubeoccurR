# Fish species check list of the Danube River Basin
species_checklist <- read.csv("./data-raw/JDS5_fish_list_v6.csv") |>
  filter(present.in.the.Danube.catchment == 'x') |>
  filter(!Species.name == "Coregonus sp") |>
  pull(Species.name)

usethis::use_data(species_checklist, overwrite = TRUE)
