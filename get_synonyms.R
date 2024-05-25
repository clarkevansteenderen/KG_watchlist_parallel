##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script reads in the WATCHLIST_INPUT_FILE.txt file in the project directory,
# and extracts the user's input parameters. This includes the file paths to
# the global list of invasive species from GRIIS, and an optional list of
# endemic species for the target country. Then:

# (1) The full list of invasive species is filtered such that species are
# removed that are already present in the target country, and that are natives

# (2) Each species is queried on GBIF to find all the synonyms and alternative
# authority names that might exist for that species. A new dataframe is
# compiled with all of these additional entries and speciesKeys to create an
# input list for the next phase of the analysis. This new dataframe (SYNONYM.LIST.csv)
# and a logfile of species that were not available from GBIF (LOGFILE.LIST.csv)
# are written to file

# (3) This script then calls "divide_data.R" to divide the data into multiple
# smaller subsets and allocate them to individual run folders

#################################################################
##                            SETUP                            ##
#################################################################

library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)

# change the divide.dataset.into value from 48 if you add more email addresses
# also then edit the usernames and email addresses

#################################################################
##                  GENERATE THE SPECIES LIST                  ##
#################################################################

# read in the input file with user-changed parameters
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

#################################################################

# extract the relevant information
griis.full = readr::read_delim(filter(input.params,
                                      row.names(input.params) %in% 
                                        c("SPECIES LIST PATH"))$choice)

endemics.list.path = dplyr::filter(input.params,
                                   row.names(input.params) %in% 
                                     c("ENDEMICS LIST PATH"))$choice

# if a path is provided, read in the endemics file
if(endemics.list.path != ""){
  # change read_delim depending on the format of the file (e.g. CSV)
  endemics.list = readr::read_delim(endemics.list.path)
  
  endemics.spp.name.col = filter(input.params,
                                 row.names(input.params) %in% 
                                   c("ENDEMICS NAME COLUMN"))$choice 
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

# Extract all the Mauritius records for Plantae that are invasive
griis.target = dplyr::filter(griis.full, 
                             checklist.name == target.country,
                             accepted_name.kingdom == target.kingdom,
                             #is_invasive == "invasive"
) %>%
  dplyr::distinct(., accepted_name.species, .keep_all = TRUE) %>%
  dplyr::arrange(., accepted_name.species) # order alphabetically

# Extract everything other than Mauritius records for Plantae that are invasive
griis.other = dplyr::filter(griis.full, 
                            checklist.name != target.country,
                            accepted_name.kingdom == target.kingdom,
                            is_invasive == "invasive") %>%
  dplyr::distinct(. , accepted_name.species, .keep_all = TRUE) %>%
  dplyr::arrange(., accepted_name.species) # order alphabetically

# remove any species in the Mauritius list (griis.target) 
# FROM the "griis.other" list
griis.other.filtered = griis.other %>%
  dplyr::filter(! accepted_name.species %in% griis.target$accepted_name.species)

# Only bother with this if there is a list of endemics available
# read in a list of native flowering plants in Mauritius, 
# and remove those records from the "griis.other.filtered" list

# if a path was provided for endemics, then remove those from the list of 
# invasive species not yet in MAU
if(endemics.list.path != ""){
  
  # remove duplicates from the endemics list
  endemics.list = endemics.list %>%
    dplyr::distinct(. , endemics.list[[endemics.spp.name.col]], .keep_all = TRUE)
  
  # store the native species that were in the griis list (and now removed)
  endemics_in_griis_list = griis.other.filtered %>%
    dplyr::filter(accepted_name.species %in% endemics.list[[endemics.spp.name.col]])
  
  # remove species in the endemics list FROM the "griis.other.filtered" list
  griis.other.filtered = griis.other.filtered %>%
    dplyr::filter(! accepted_name.species %in% endemics.list[[endemics.spp.name.col]])
  
}# if

######################################################################
#             Get all synonyms for each input species                #
######################################################################

SYNONYM.LIST = c()
LOGFILE.LIST = c()

SPECNAMES = griis.other.filtered %>%
  dplyr::select(spp.name.column) %>%
  as.data.frame()

# small subset to test
#SPECNAMES = dplyr::slice(SPECNAMES, c(1:5))

for(t in 1:nrow(SPECNAMES)){
  
  synonyms.df = c()
  
  tryCatch({
    
    message(paste0("Getting synonyms for ", SPECNAMES[t,], ": ", t, " of ", 
                   nrow(SPECNAMES)))
    
    synonyms.df = rgbif::name_backbone_checklist(SPECNAMES[t,], verbose=T) %>%
      dplyr::select(usageKey, scientificName, status, matchType,
                    canonicalName, rank, species, speciesKey) %>%
      dplyr::filter(rank == "SPECIES", matchType %in% c("EXACT", "FUZZY")) %>%
      # remove all duplicate speciesKey rows
      dplyr::distinct(speciesKey, .keep_all = TRUE)
  }, 
  error = function(e) {
    message(paste0("\nAn error occurred while fetching GBIF information for ", 
                   SPECNAMES[t,], ": ", 
        conditionMessage(e), " ...moving on\n"))
    
    return(NULL)  # Return NULL if usageKey is not found
  })
  
  # Check if usageKey is NULL, indicating an error occurred
  if (is.null(synonyms.df)) {
    LOGFILE.LIST = c(LOGFILE.LIST, SPECNAMES[t,])
    # Move on to the next iteration rather than breaking the loop
    next
  }# if
  
  synonyms.df$num.recs = NA
  
  for(p in 1:nrow(synonyms.df)){
    synonyms.df$num.recs[p] = rgbif::occ_count(speciesKey = synonyms.df$speciesKey[p])
  }#for
  
  synonyms.df = synonyms.df %>% dplyr::filter(., num.recs != 0)
  
  synonyms.df$original.input.sp = SPECNAMES[t,]
  
  SYNONYM.LIST = dplyr::bind_rows(SYNONYM.LIST, synonyms.df)

}#for

message(paste0("\nYour species list changed from ", nrow(SPECNAMES), " to ",
               nrow(SYNONYM.LIST), " entries \n"))

# SYNONYM.LIST is now the input list to use for the rest of the analysis
# we can use the speciesKeys straight up now

write.csv(SYNONYM.LIST, "FILTERED_SYNONYMS_INC_INPUT_DATA.csv", row.names = FALSE)

# if the log file is not empty, write to PC
if(!is.null(LOGFILE.LIST)){
write.csv(as.data.frame(LOGFILE.LIST), "NO_GBIF_RECS.csv", row.names = FALSE)
}#if

message(paste0("These species did not have GBIF records: ",
               paste(LOGFILE.LIST, collapse = ", ") ))
