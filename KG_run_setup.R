##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script reads in the PARAMETERS.csv file with the info needed to
# download from GBIF

# The input species list is the one that contains all synonyms from GBIF,
# and uses the speciesKey of each to download from GBIF

# In this approach, the whole list of speciesKey numbers are supplied at once
# as a vector, and GBIF generates one very large output zipped folder with all
# the results (the Mauritius folder contained 180,575,480 records for 4002 spp)

# The folder is downloaded to OUTPUTS/GBIF_DATA

# The next steps need to be run on the Linux command line before the last
# R script can be run

# The GBIF folder is enormous, and can't be read into R as one file. The folder
# needs to be unzipped and divided into multiple smaller files. Here, I've 
# called them chunk_n, where n = 1 to about 180-200 subfiles. Dividing each
# file into 1 million rows works well

# The column headers then need to be appended back onto each subset

# Now the KG_run.R script can be run, taking each chunk_n file in turn and
# applying coarse climate matching based on shared KG zones

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
library(vroom)

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

#########################################################################

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
  message("\n✔ CREATED FOLDER OUTPUTS/GBIF_DATA...\n")
}

#########################################################################
# Record the start time
#########################################################################
start_time = Sys.time()
#########################################################################

message(paste0("DOWNLOAD STARTED AT ", start_time, "\n"))

# Download records for the taxon keys

message("\n✔ DOWNLOADING FROM GBIF...\n")

gbif_download = rgbif::occ_download(
  # exclude any records with geospatial issues
  rgbif::pred("hasGeospatialIssue", FALSE),
  # keep only records with available GPS coordinates
  rgbif::pred("hasCoordinate", TRUE),
  # remove absent records
  rgbif::pred("occurrenceStatus","PRESENT"), 
  # remove fossil and living (zoos and botanical gardens) observations
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  rgbif::pred_in("speciesKey", species_keys),
  format = "SIMPLE_CSV",
  user = user.input$gbif.username[1],
  pwd = user.input$gbif.password[1],
  email = user.input$gbif.email[1]
)

rgbif::occ_download_wait(gbif_download, 
                         quiet = FALSE, 
                         status_ping = 3) 

result = rgbif::occ_download_get(key = gbif_download, 
                                 overwrite = TRUE, path = paste0("OUTPUTS/GBIF_DATA/"))

#########################################################################
# Record end time
#########################################################################
end_time = Sys.time()
#########################################################################
# Calculate the time taken
#########################################################################
message("TASK STARTED AT: \n", start_time, "\nTASK COMPLETED AT: \n",
        end_time)
#########################################################################

# AT THIS POINT, THE ZIPPED FILE NEEDS TO BE UNZIPPED AND SPLIT TO MAKE IT MANAGEABLE
# NEEDS TO BE DONE IN LINUX FIRST

############################# STEPS #################################

############ IN LINUX ##############

# # unzip the gbif download
# # extract the header columns from the unzipped CSV
# # split the csv into separate smaller files, specifying the number of rows in each
# # add the header back onto each file
# 
############ IN R ##################
# 
# nohup Rscript KG_run.R &> KG_run.out &
