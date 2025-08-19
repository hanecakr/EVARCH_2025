werkjaar <- 2025
rapport <- "EVARCH25_"

archeoregios <- readRDS(here::here("data_processed/archeoregios.rds"))
provincies <- readRDS(here::here("data_processed", "provincies.rds"))
gemeentes <- readRDS(here::here("data_processed", "gemeentes.rds"))

AP_docs_sf <- readRDS(here::here("data_processed", "AP_docs.rds"))
AP_docs <- AP_docs_sf |> sf::st_drop_geometry() 

# Package availability check
required_packages <- c(
  "tidyverse",
  "gt",
  "readxl",
  "writexl",
  "here",
  "roeference",
  "sf",
  "patchwork",
  "scales",
  "ggspatial")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "), 
             "\nInstall with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))", sep = ""))
}
