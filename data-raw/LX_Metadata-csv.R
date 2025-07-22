## code to prepare `LX_Metadata.csv` dataset goes here

lx_data <- readr::read_csv(
  file = system.file("extdata/LX_Metadata.csv", package = "LevelCrossingExplorer"),
  name_repair = janitor::make_clean_names
)

usethis::use_data(lx_data, overwrite = TRUE)
