##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script sets everything up for the analysis to start running, and is
# sourced by the KG_run.R script. 

# The koppen-geiger shape file is read in, as well as the subsetted species
# list present in that particular RUNS/RUN(n) folder

# The rest of the script sets up the data frame with its necessary columns
# to which the summary data will be written 

#########################################################################
# Session setup 1
#########################################################################

library(tidyverse)
library(rgbif)
library(terra)
library(tidyr)
library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(purrr)

#########################################################################
# Session setup 2
#########################################################################

run.folder = "OUTPUTS/PARAMETERS.csv"

#########################################################################

user.input = read.csv(run.folder)

iso.country.code = user.input$iso.country.code %>%
  purrr::keep(nzchar) # nzchar discards empty characters

kopgeig.zone.nums = user.input$koppengeiger.zone.numbers %>%
  purrr::keep(nzchar)

output_file_name = user.input$output.file.name  %>%
  purrr::keep(nzchar)

#########################################################################

#########################################################################
# Import KG map
#########################################################################

kg_map <- terra::rast("koppen_geiger/Beck_KG_V1_present_0p0083.tif")

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format 
terra::crs(kg_map) = "epsg:4326"
terra::crs(kg_map, describe = T)

# The list of species to assess
file_url = user.input$spp.file.path[1]

watchlist_file = read.csv(file_url)
total_taxa = nrow(watchlist_file)

species_names = watchlist_file %>% 
  dplyr::pull(species) 

species_keys = watchlist_file %>% 
  dplyr::pull(speciesKey) 

#########################################################################
# create these folders the first time the script is run, or if they
# no longer exist
#########################################################################

if (!dir.exists("OUTPUTS/GBIF_DATA")) {
  dir.create("OUTPUTS/GBIF_DATA")
}
