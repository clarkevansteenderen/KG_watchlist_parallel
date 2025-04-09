##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This short script creates a CSV file that contains the input
# information needed to start downloading from GBIF. It also reads in
# the WATCHLIST_INPUT_FILE.txt input file that the user has edited.
# The output parameter file is saved as "PARAMETERS.csv"

#################################################################
##                            SETUP                            ##
#################################################################

library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)

#################################################################
##                    READ INPUT FILE                          ##
#################################################################

# read in the input file with user-changed parameters
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

input.params$choice = trimws(input.params$choice)

# read in iso country codes
iso.codes = read.csv("iso_codes/iso_codes.csv")

####################################################################
##    extract  info from the user's input file                    ##
####################################################################

country.name = filter(input.params,
                          row.names(input.params) %in% 
                            c("TARGET COUNTRY"))$choice

iso.country.code = iso.codes %>% 
  dplyr::filter(stringr::str_detect(tolower(Name), tolower(country.name))) %>%
  pull(Code)

message(paste0("\n✔ THE COUNTRY CODE FOR ", country.name, " IS ", iso.country.code))

# koppengeiger.zones = filter(input.params,
#                             row.names(input.params) %in% 
#                               c("KOPPEN-GEIGER ZONES"))$choice 
# 
# koppengeiger.zones = as.numeric(unlist(strsplit(koppengeiger.zones, ",\\s*")))

# high risk (HR) countries of interest (e.g. trading partners)
HR.iso.country = filter(input.params,
                            row.names(input.params) %in% 
                              c("HIGH RISK COUNTRY/IES"))$choice 

HR.iso.country = unlist(strsplit(HR.iso.country, ",\\s*"))

get_country_code = function(country_name) {
  match <- iso.codes %>%
  dplyr::filter(stringr::str_detect(tolower(Name), tolower(country_name))) %>%
    slice(1) %>%  # Take only the first match
    pull(Code)
  if (length(match) == 0) return("UNKNOWN") else return(match)
}

# Apply function to all country names
HR.country.codes = purrr::map_chr(HR.iso.country, get_country_code)
# remove any potential Unknown country acronyms
HR.country.codes = HR.country.codes[HR.country.codes != "UNKNOWN"]

# Combine into a data frame
HR.results = data.frame(Country = HR.iso.country, ISO_Code = HR.country.codes)

HR.iso.country.code = HR.results$ISO_Code

message(paste0("\n✔ HIGH RISK COUNTRY ISO CODES ARE: ", 
               paste(HR.iso.country.code, collapse = ", ")))

gbif.username = filter(input.params,
                       row.names(input.params) %in% 
                         c("GBIF USERNAME"))$choice

gbif.email = filter(input.params,
                    row.names(input.params) %in% 
                      c("GBIF EMAIL"))$choice

gbif.password = filter(input.params,
                       row.names(input.params) %in% 
                         c("GBIF PASSWORD"))$choice

#########################################################################
## GET THE KOPPEN GEIGER MAP, AND EXTRACT CLIMTATE TYPES FOR THE TARGET
## COUNTRY AUTOMATICALLLY
#########################################################################

kg_map <- terra::rast("koppen_geiger/Beck_KG_V1_present_0p0083.tif")
message("\n✔ READ IN KOPPEN-GEIGER SHAPE FILE...\n")

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format 
terra::crs(kg_map) = "epsg:4326"
terra::crs(kg_map, describe = TRUE)

country = rnaturalearth::ne_countries(scale = "medium", country = country.name, returnclass = "sf")
kg_country = terra::crop(kg_map, country)   # Crop to country extent
kg_country = terra::mask(kg_country, country)  # Mask outside areas
#terra::plot(kg_country, legend = TRUE)

climate_types = unique(terra::values(kg_country), na.rm = TRUE)

koppengeiger.zones = as.numeric(climate_types) %>%
  na.omit() %>% # remove NA
  .[. != 0] %>% # remove 0
  sort() # sort in ascending order

message(paste0("\n✔ KOPPEN-GEIGER CLIMATE ZONES FOR ", country.name, " ARE: ", 
               paste(koppengeiger.zones, collapse = ", ")))


#########################################################################
## GENERATE CUSTOMISED INPUT.CSV FILE                                  ##
#########################################################################

message("\n✔ CREATING INPUT FILE...\n")

input.file.template = data.frame(matrix(ncol = 7))
colnames(input.file.template) = c("spp.file.path", 
                                  "gbif.username", "gbif.password", "gbif.email",
                                  "iso.country.code", "HR.iso.country.code",
                                  "koppengeiger.zone.numbers")

##############################################################################


# Determine the number of rows needed based on the longest vector ->
# country codes or koppen zones could have multiple entries
num_rows = max(length(iso.country.code), length(koppengeiger.zones),
               length(HR.iso.country.code))

# dynamically set the number of rows to the number of koppengeiger zones
input.file.template = input.file.template[rep(1, num_rows), ]
rownames(input.file.template) = seq_len(nrow(input.file.template))

##############################################################################

# Now set the constant variables that will be the same for all files
# Assign the variables to the columns, ensuring proper lengths

if (length(iso.country.code) == 1) {
  input.file.template$iso.country.code[1] = iso.country.code
} else {
  input.file.template$iso.country.code = iso.country.code
}

if (length(koppengeiger.zones) == 1) {
  input.file.template$koppengeiger.zone.numbers[1] = koppengeiger.zones
} else {
  input.file.template$koppengeiger.zone.numbers[1:length(koppengeiger.zones)] = koppengeiger.zones
}

if (length(HR.iso.country.code) == 1) {
  input.file.template$HR.iso.country.code[1] = HR.iso.country.code
} else {
  input.file.template$HR.iso.country.code[1:length(HR.iso.country.code)] = HR.iso.country.code
}

##############################################################################

##############################################################################
# now loop through each folder to create a customised INPUT file, with the
# correct gbif username and password
##############################################################################

  # create a copy of the template
  INPUT = input.file.template
  # set the remaining columns
  INPUT$spp.file.path[1] = "OUTPUTS/FILTERED_SYNONYMS_INC_INPUT_DATA.csv"
  INPUT$gbif.username[1] = gbif.username
  INPUT$gbif.password[1] = gbif.password
  INPUT$gbif.email[1] = gbif.email
  # remove NAs
  INPUT[is.na(INPUT)] = ""
  
  # write the INPUT file
  write.csv(INPUT, 
            paste0("OUTPUTS/PARAMETERS.csv"),
            row.names = FALSE)

##############################################################################

message("\n✔✔ PREPARATIONS DONE ✔✔\n")
