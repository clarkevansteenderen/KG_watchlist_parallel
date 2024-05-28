##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

#################################################################
##                            SETUP                            ##
#################################################################

library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)

#################################################################
##                    READ INPUT FILE                          ##
#################################################################

# read in the input file with user-changed parameters
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

####################################################################
##    extract  info from the user's input file                    ##
####################################################################

iso.country.code = filter(input.params,
                          row.names(input.params) %in% 
                            c("ISO COUNTRY CODE"))$choice

koppengeiger.zones = filter(input.params,
                            row.names(input.params) %in% 
                              c("KOPPEN-GEIGER ZONES"))$choice 

koppengeiger.zones = as.numeric(unlist(strsplit(koppengeiger.zones, ",\\s*")))

output.file.name = filter(input.params,
                          row.names(input.params) %in% 
                            c("OUTPUT FILE NAME"))$choice

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
##                      READ IN SPECIES LIST                           ##
#########################################################################

SYNONYM.LIST = read.csv("OUTPUTS/FILTERED_SYNONYMS_INC_INPUT_DATA.csv")

#########################################################################
#########################################################################

#########################################################################
## GENERATE CUSTOMISED INPUT.CSV FILE                                  ##
#########################################################################

message("\nCREATING INPUT FILE...")

input.file.template = data.frame(matrix(ncol = 7))
colnames(input.file.template) = c("spp.file.path", 
                                  "gbif.username", "gbif.password", "gbif.email",
                                  "iso.country.code", "koppengeiger.zone.numbers",
                                  "output.file.name")

##############################################################################

# Determine the number of rows needed based on the longest vector ->
# country codes or koppen zones could have multiple entries
num_rows = max(length(iso.country.code), length(koppengeiger.zones))

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

if (length(output.file.name) == 1) {
  input.file.template$output.file.name[1] = output.file.name
} else {
  input.file.template$output.file.name = output.file.name
}

if (length(koppengeiger.zones) == 1) {
  input.file.template$koppengeiger.zone.numbers[1] = koppengeiger.zones
} else {
  input.file.template$koppengeiger.zone.numbers = koppengeiger.zones
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
  INPUT$gbif.email[1] = gbif.emailaddress
  # remove NAs
  INPUT[is.na(INPUT)] = ""
  
  # write the INPUT file to each RUN folder
  write.csv(INPUT, 
            paste0("OUTPUTS/PARAMETERS.csv"),
            row.names = FALSE)

##############################################################################

message("\nPREPARATIONS DONE :)")
