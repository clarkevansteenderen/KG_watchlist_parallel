##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script reads in the WATCHLIST_INPUT_FILE.txt file in the project directory,
# and extracts the user's input parameters. This includes the file paths to
# the global list of invasive species from GRIIS, and an optional list of
# endemic species for the target country. Then:

# (1) The full list of invasive species is filtered such that species are
# removed that are already present in the target country, and native species

# (2) Each species is queried on GBIF to find all the synonyms and alternative
# authority names that might exist for that species. A new dataframe is
# compiled with all of these additional entries and speciesKeys to create an
# input list for the next phase of the analysis. This new dataframe 
# (FILTERED_SYNONYMS_INC_INPUT_DATA.csv.csv) and a logfile of species 
# that were not available from GBIF (NO_GBIF_RECS.csv) are written to file

# Getting all synonyms from GBIF can take a few minutes

#################################################################
##                            SETUP                            ##
#################################################################

library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)

source("gbif.synonyms.function.R")

if(!dir.exists("OUTPUTS")){
  dir.create("OUTPUTS")
}

####################################################################
# Record starting time
####################################################################
start_time = Sys.time()
####################################################################

#################################################################
##                  GENERATE THE SPECIES LIST                  ##
#################################################################

# read in the input file with user-changed parameters

input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

input.params$choice = trimws(input.params$choice)

#################################################################

message("\n✔ READING IN INVASIVE SPECIES...\n")

# extract the relevant information
griis.full = readr::read_delim(filter(input.params,
                                      row.names(input.params) %in% 
                                        c("SPECIES LIST PATH"))$choice)

# Filter rows where 'accepted_name.habitat' contains "terrestrial"
# ie. remove all other habitat types
griis.full = griis.full[grepl("terrestrial", griis.full$accepted_name.habitat), ]
#griis.full$accepted_name.habitat = as.factor(griis.full$accepted_name.habitat)
#levels(griis.full$accepted_name.habitat)

# griis.plantae = griis.full %>% dplyr::filter(accepted_name.kingdom == "Plantae")
 
exclude.list.path = dplyr::filter(input.params,
                                   row.names(input.params) %in% 
                                     c("SPECIES TO EXCLUDE PATH"))$choice

# if a path is provided, read in the exclude file
if(exclude.list.path != ""){
  
  message("\n✔ READING IN SPECIES TO REMOVE...\n")
  # change read_delim depending on the format of the file (e.g. CSV)
  exclude.list = read.csv(exclude.list.path)
  
}#if

target.country = filter(input.params,
                        row.names(input.params) %in% 
                          c("TARGET COUNTRY"))$choice

# this might need to be ignored, depending on the desired outcome
target.kingdom = filter(input.params,
                        row.names(input.params) %in% 
                          c("KINGDOM"))$choice

####################################################################
##  # extract more info from the user's input file  ##
####################################################################

spp.name.column = filter(input.params,
                         row.names(input.params) %in% 
                           c("SPECIES NAME COLUMN"))$choice

#################################################################
##                  GENERATE SPECIES LIST                  ##
#################################################################

message(paste0("\n✔ FILTERING BY: \nCountry: ",
               target.country, " \nTaxonomic kingdom: ", target.kingdom, 
               "\nRemoving duplicates"))

griis.invasive.orgs = griis.full %>% 
    dplyr::filter(accepted_name.kingdom == target.kingdom, 
                  is_invasive == "invasive") 

message(paste0("\n✔ THERE ARE ", nrow(griis.invasive.orgs), 
               " INVASIVE ", target.kingdom))

# Extract all the Mauritius records for Plantae that are invasive
griis.target = dplyr::filter(griis.full, 
                             checklist.name == target.country,
                             accepted_name.kingdom == target.kingdom,
                             is_invasive == "invasive"
) %>%
  dplyr::distinct(., accepted_name.species, .keep_all = TRUE) %>%
  dplyr::arrange(., accepted_name.species) # order alphabetically

message(paste0("\n✔ FOUND ", nrow(griis.target), 
               " INVASIVE SPECIES RECORDS IN ", target.country))

# Extract everything other than Mauritius records for Plantae that are invasive
griis.other = dplyr::filter(griis.full, 
                            checklist.name != target.country,
                            accepted_name.kingdom == target.kingdom,
                            is_invasive == "invasive"
                            ) %>%
  dplyr::distinct(. , accepted_name.species, .keep_all = TRUE) %>%
  dplyr::arrange(., accepted_name.species) # order alphabetically

message(paste0("\n✔ FOUND ", nrow(griis.other), 
               " INVASIVE SPECIES RECORDS THAT EXCLUDE ", target.country))

message(paste0("\n✔ REMOVING SPECIES ALREADY PRESENT IN ",
               target.country))

# remove any species in the Mauritius list (griis.target) 
# FROM the "griis.other" list
griis.other.filtered = griis.other %>%
  dplyr::filter(! accepted_name.species %in% griis.target$accepted_name.species)

message(paste0("\n✔ THERE ARE NOW ", nrow(griis.other.filtered) , 
               " INVASIVE SPECIES RECORDS THAT EXCLUDE TAXA THAT HAVE BEEN RECORDED IN ", 
               target.country))

# Only bother with this if there is a list of endemics/agric/pests available to exclude
# read in a list of native flowering plants in Mauritius, 
# and remove those records from the "griis.other.filtered" list

# if a path was provided for spp. to exclude, then remove those from the list of 
# invasive species not yet in the target country
if(exclude.list.path != ""){
  
  message(paste0("\n✔ REMOVING EXCLUDED SPECIES FROM THE LIST OF INVASIVES..."))
  
  # remove duplicates from the list
  exclude.list = exclude.list %>%
    dplyr::distinct(. , exclude.list[1], .keep_all = F)
  
  message(paste0("\n✔ THERE ARE ", nrow(exclude.list), " SPECIES IN THE LIST PROVIDED"))
  
  # test
  #exclude.list = slice(exclude.list, c(1:5))
  
  # get all the synonyms from the list
  exclude.synonyms = gbif.synonyms(spp.list = exclude.list)
  exclude.list.full = exclude.synonyms$SYNONYM.LIST
  
  write.csv(exclude.list.full, "OUTPUTS/SPECIES_TO_EXCLUDE_WITH_SYNONYMS.csv", 
            row.names = FALSE)
  
  # if the log file is not empty, write to PC
  if(!is.null(exclude.synonyms$LOGFILE.LIST)){
    write.csv(as.data.frame(exclude.synonyms$LOGFILE.LIST), 
              "OUTPUTS/SPECIES_TO_EXCLUDE_WITH_SYNONYMS_LOGFILE.csv", row.names = FALSE)
    message("\nLOG FILE WRITTEN TO OUTPUTS/SPECIES_TO_EXCLUDE_WITH_SYNONYMS_LOGFILE.csv")
    
    message(paste0("\nTHESE SPECIES DID NOT HAVE GBIF RECORDS: \n",
                   paste(exclude.synonyms$LOGFILE.LIST, collapse = "\n") ))
  }#if
  
  # store the species that were in the griis list (and now removed)
  excluded_spp_in_griis_list = griis.other.filtered %>%
    dplyr::filter(accepted_name.species %in% exclude.list.full$species)
  
  if(nrow(excluded_spp_in_griis_list) > 0){
  message(paste0("\n✔ ", nrow(excluded_spp_in_griis_list), 
                 " SPECIES WERE REMOVED FROM THE INVASIVES LIST"))
  }#if
  
  if(nrow(excluded_spp_in_griis_list) == 0){
    message("\n✔ THERE WERE NO MATCHING EXCLUDED SPECIES IN THE LIST OF INVASIVES")
  }# else if
  
  # remove species in the exclude list FROM the "griis.other.filtered" list
  griis.other.filtered = griis.other.filtered %>%
    dplyr::filter(! accepted_name.species %in% exclude.list.full$species)
  
  message(paste0("\n✔ THERE ARE NOW ", nrow(griis.other.filtered), 
                 " SPECIES IN THE INVASIVE LIST THAT WILL BE USED IN THE NEXT STEP"))
  
}# if

######################################################################
#             Get all synonyms for each input species                #
######################################################################

message("\n✔ SEARCHING FOR TAXONOMIC SYNONYMS AND ALTERNATIVE AUTHORITY NAMES...\n")

SPECNAMES = griis.other.filtered %>%
  dplyr::select(all_of(spp.name.column)) %>%
  as.data.frame()

# small subset to test
# SPECNAMES = dplyr::slice(SPECNAMES, c(1:2))

# run function to get synonyms
griis.synonyms = gbif.synonyms(spp.list = SPECNAMES)

write.csv(griis.synonyms$SYNONYM.LIST, "OUTPUTS/FILTERED_SYNONYMS_INC_INPUT_DATA.csv", 
          row.names = FALSE)

# if the log file is not empty, write to PC
if(!is.null(griis.synonyms$LOGFILE.LIST)){
  write.csv(as.data.frame(griis.synonyms$LOGFILE.LIST), 
            "OUTPUTS/NO_GBIF_RECS.csv", row.names = FALSE)
  message("\nLOG FILE WRITTEN TO OUTPUTS/NO_GBIF_RECS.csv")
  
  message(paste0("\nTHESE SPECIES DID NOT HAVE GBIF RECORDS: \n",
                 paste(griis.synonyms$LOGFILE.LIST, collapse = "\n") ))
}#if


#########################################################################
# Record end time
#########################################################################
end_time = Sys.time()
#########################################################################
# Calculate the time taken
message("\nTASK STARTED AT: \n", start_time, "\nTASK COMPLETED AT: \n",
        end_time)
#########################################################################

