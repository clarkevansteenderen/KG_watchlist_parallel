##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script sets everything up for the analysis to start running, and is
# sourced by the KG_run.R script. 
# It first specifies the relevant RUNS/RUN(n) folder in which to fetch the
# input invasive species list (subsetted) and the INPUT.csv file with the 
# required input parameters provided by the user in the 
# WATCHLIST_INPUT_FILE.txt file

# The arg that is taken in is just a number, indicating the RUN(n)
# subfolder number (e.g. RUNS/RUN1, or RUNS/RUN48). This is needed when this
# script is run from the command line. For example:
# > Rscript KG_run.R 1

# The koppen-geiger shape file is read in, as well as the subsetted species
# list present in that particular RUNS/RUN(n) folder

# The rest of the script sets up the data frame with its necessary columns
# to which the summary data will be written as the loop progresses through
# all the species names in the list

# A download_records function is declared to access the GBIF data for each
# species as the loop progresses, and a log file is declared to store species
# where errors occurred and the download was skipped

# Folders are written: data/zip and results/summary, into which both temporary
# and permanent output files are written

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

# when this script is run from the command line, take in a parameter to 
# specify the folder you want to run, example the RUN1 folder:
#    >     Rscript KG_run.R 1
# on the HPC:
#    >     nohup Rscript KG_run.R 1 &> RUNS/RUN1/RUN1.out &
# change 1 to 2, 3, etc
# just make sure that the INPUT.csv folder is always called INPUT.csv and not 
# changed!

# gets the arg from the commandline
arg = commandArgs(trailingOnly = TRUE)
arg = paste0("RUN", arg)
# sets the desired folder
run.folder = paste0("RUNS/", arg, "/INPUT.csv")

#########################################################################

user.input = read.csv(run.folder)

iso.country.code = user.input$iso.country.code %>%
  purrr::keep(nzchar) # nzchar discards empty characters

kopgeig.zone.nums = user.input$koppengeiger.zone.numbers %>%
  purrr::keep(nzchar)

output_file_name = user.input$output.file.name  %>%
  purrr::keep(nzchar)

keep.folders = user.input$keep.downloads %>%
  purrr::keep(nzchar) %>%
  tolower()

#########################################################################

#########################################################################
# Import KG map
#########################################################################

kg_map <- terra::rast("koppen_geiger/Beck_KG_V1_present_0p0083.tif")

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format 
terra::crs(kg_map) = "epsg:4326"
terra::crs(kg_map, describe = T)

#########################################################################
# Get GPS records from GBIF
#########################################################################

# The list of species to assess
file_url = user.input$spp.file.path[1]

watchlist_file = read.csv(file_url)
total_taxa = nrow(watchlist_file)

num_species_to_process = user.input$num.spp[1]

if(!is.numeric(num_species_to_process)){
  num_species_to_process = total_taxa
}

species_names = watchlist_file %>% 
  dplyr::slice(1:num_species_to_process) %>% 
  dplyr::pull(species[1]) 

species_keys = watchlist_file %>% 
  dplyr::slice(1:num_species_to_process) %>% 
  dplyr::pull(speciesKey[1]) 

#########################################################################
# create output table
#########################################################################

# always 8 columns by default, accounting for species, total_n,
# total_records_in_kg, prop_records_in_kg, n_records_in_target_countries,
# mins (for each file download), and file size (in MB)

# the rest need to be added depending on which climate zones the user has
# specified

ncol.super.table = 8 + (length(kopgeig.zone.nums)*2) # *2 because we have 
#total_records and prop_records per zone

super_table = data.frame(matrix(ncol = ncol.super.table, 
                                nrow = num_species_to_process))

colnames_supertable <- c()
climate_zone_names <- c()

names_to_add <- c("species", "total_n", "total_records_in_kg", 
                  "prop_records_in_kg")

colnames_supertable <- c(names_to_add, colnames_supertable)

# Loop over each number in kopgeig.zone.nums
for (num in kopgeig.zone.nums) {
  # Generate names for total records
  total_name <- paste("total_records_in_", num, sep = "")
  # Generate names for proportion records
  prop_name <- paste("prop_records_in_", num, sep = "")
  # Add the generated names to the vector
  colnames_supertable = c(colnames_supertable, total_name, prop_name)
  # store a copy of just the climate zone names
  climate_zone_names = c(climate_zone_names, total_name, prop_name)
}

names_to_add_end = c("n_records_in_target_countries", 
                     "citation", "mins", "size.mb")

# Concatenate the names to add at the end
colnames_supertable = c(colnames_supertable, names_to_add_end)

colnames(super_table) = colnames_supertable

#########################################################################
# Function to download records for a single taxon key
#########################################################################

download_records = function(K) {
  rgbif::occ_download(
    # could also be taxonKey?
    rgbif::pred_in("speciesKey", K),
    format = "SIMPLE_CSV",
    user = user.input$gbif.username[1],
    pwd = user.input$gbif.password[1],
    email = user.input$gbif.email[1]
  )
}

#########################################################################
# create these folders the first time the script is run, or if they
# no longer exist
#########################################################################

if (!dir.exists(paste0("RUNS/", arg, "/data"))) {
  dir.create(paste0("RUNS/", arg, "/data"))
}

if (!dir.exists(paste0("RUNS/", arg, "/data/zip"))) {
  dir.create(paste0("RUNS/", arg, "/data/zip"))
}

if (!dir.exists(paste0("RUNS/", arg, "/results"))) {
  dir.create(paste0("RUNS/", arg, "/results"))
}

if (!dir.exists(paste0("RUNS/", arg, "/results/summary"))) {
  dir.create(paste0("RUNS/", arg, "/results/summary"))
}

#########################################################################
# keep a log of possible problem runs
#########################################################################

error.log = c()
