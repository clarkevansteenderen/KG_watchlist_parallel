##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This is the meat of the program, where climate matching is done

# The Koppen-Geiger map is read in

# Each chunk_n.csv file (each subset of the large GBIF file) is read in to a 
# loop, where all the records in that file are climate-matched to the KG
# zones in the target country

# A summary table is produced for each chunk, and saved in a list

# All the summary tables in the list are collated at the end, so that there
# is one big output summary table

# The table is processed so that all the same species name entries are summed,
# and the proportion for each KG zone is recorded

# Two final watchlists are produced: one that has retained all the synonyms for
# each species (where applicable), and one where synonyms have been lumped together
# under one name, and their KG scores summed

##################################################################

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

run.folder = "OUTPUTS/PARAMETERS.csv"

#########################################################################

user.input = read.csv(run.folder) 

iso.country.code = user.input$iso.country.code %>%
  purrr::keep(nzchar) # nzchar discards empty characters

# high risk countries of interest (e.g. trading partners)
HR.iso.country.code = user.input$HR.iso.country.code %>%
  purrr::keep(nzchar) %>%
  purrr::keep(~ !is.na(.))

kopgeig.zone.nums = user.input$koppengeiger.zone.numbers %>%
  purrr::keep(nzchar) %>%
  purrr::keep(~ !is.na(.))

# user's input file
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

target.country = filter(input.params,
                        row.names(input.params) %in% 
                          c("TARGET COUNTRY"))$choice

#########################################################################
# Import KG map
#########################################################################

kg_map <- terra::rast("koppen_geiger/Beck_KG_V1_present_0p0083.tif")
message("\n✔ READ IN KOPPEN-GEIGER SHAPE FILE...\n")

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format 
terra::crs(kg_map) = "epsg:4326"
terra::crs(kg_map, describe = T)

#########################################################################

TABLE.LIST = list()

chunk_files = list.files(path = "OUTPUTS/GBIF_DATA/", pattern = "^chunk_")

start_time = Sys.time()

message("\n✔ STARTING TO READ IN DATA CHUNKS...")

for(p in 1:length(chunk_files)){
  
  message(paste0("\nREADING IN GBIF DOWNLOAD CHUNK ", p, " OF ",
                 length(chunk_files), "\n"))
  
  # CHUNK.DATA = vroom::vroom(paste0("OUTPUTS/GBIF_DATA/", chunk_files[p]),
  #                           col_select = c("order", "family", "species",
  #                                          "scientificName", "countryCode", "speciesKey",
  #                                          "decimalLatitude", "decimalLongitude"))
  
  # readr less likely to produce warnings, vroom has some issues
  CHUNK.DATA = 
    #readr::read_delim(file = "OUTPUTS/chunk_01.csv",
    readr::read_delim(file = paste0("OUTPUTS/GBIF_DATA/", chunk_files[p]),
                      col_select = c(order, family, species,
                                     scientificName, countryCode, speciesKey,
                                     decimalLatitude, decimalLongitude))
  
  # CHUNK.DATA = CHUNK.DATA %>%
  #   dplyr::filter(countryCode ==  c("BE", "HU"))
  
  message("\n✔ FILE READ IN SUCCESSFULLY")
  
  ##########################################################################
  # Koppen-Geiger analysis for global records 
  ##########################################################################
  
  # Keep all other records not in target country/ies
  df = CHUNK.DATA %>%
    dplyr::select(
      order,
      family,
      species,
      scientific_name = scientificName,
      country = countryCode,
      lat = decimalLatitude,
      lon = decimalLongitude
    ) %>%
    # Remove records from target country/ies
    dplyr::filter(!country %in% iso.country.code)
  
  message("\n✔ REMOVING RECORDS WITH MISSING GPS RECORDS...")
  
  # Drop rows with no GPS data -> this can result in columns with NAs.
  df = df %>% 
    tidyr::drop_na(lat, lon)
  
  message("\n✔ REMOVING DUPLICATE GPS RECORDS...")
  
  # Remove any duplicate GPS points 
  df = df %>%
    dplyr::distinct(lat, lon, .keep_all = TRUE)
  
  # make sure that lat and lon are numeric - some GBIF files have one or both
  # as characters for some odd reason!
  df$lat = as.numeric(df$lat)
  df$lon = as.numeric(df$lon)
  
  ##########################################################################
  # Are any records already known from the target country?
  ##########################################################################
  
  message("\n✔ CHECKING FOR RECORDS ALREADY IN ", target.country)
  
  # Set aside records from target country/ies for later 
  # note here that the input is df, not COMBO.DF
  # df has all duplicate GPS points removed, while COMBO.DF has everything
  # took a while to figure out that this was why the total_records_in_country
  # e.g. total_records_in_ZA sometimes had more than the total_n value!
  
  target_records = df %>%
    dplyr::filter(country %in% iso.country.code)
  
  # Calculate if any records are already in target country/ies
  target_records_summed = target_records %>%
    dplyr::group_by(order, family, species, .drop = FALSE) %>% # added: order, family,
    dplyr::summarise(
      total_records_in_target_country = n()
    )
  
  ##########################################################################
  # Are any records from other high risk countries, as specified by user?
  ##########################################################################
  
  if(length(HR.iso.country.code) > 0){
    
    HR.list = list()
    
    for(h in 1:length(HR.iso.country.code)){
      
      # here, df is also taken in rather than COMBO.DF
      highriskcountry_records = df %>%
        dplyr::filter(country %in% HR.iso.country.code[h])
      
      country.col.name = paste0("total_records_in_", HR.iso.country.code[h])
      
      HR.list[[h]] = highriskcountry_records %>%
        dplyr::group_by(order, family, species, .drop = FALSE) %>%
        dplyr::summarise(
          !!sym(country.col.name) := n()
        )
      
    }#for
    
  }#if
  
  message("\n✔ EXTRACTING CLIMATE AT GPS LOCALITIES...")
  
  # Extract climate at these points 
  kg_extract = terra::extract(
    x = kg_map,    # SpatRast containing climate and topo layers
    y = dplyr::select(df, c(lon, lat))  # SpatVect or data.frame containing GPS of study taxon (lon, lat)
  )
  
  # Join KG layer with GPS data 
  data =
    dplyr::bind_cols(df, kg_extract) %>%
    dplyr::rename(
      kg_zone = Beck_KG_V1_present_0p0083
    ) %>%
    tidyr::drop_na(kg_zone) %>%
    dplyr::select(-c(ID))
  
  message("\n✔ CLASSIFYING CLIMATE TYPE AT EACH GPS LOCALITY...")
  
  # Classify if each GPS point lies in a KG zone present 
  # in the target country/ies
  # these are specified in the INPUT.csv file edited by the user
  data_kg = data %>%
    dplyr::mutate(kg_score = dplyr::case_when(
      kg_zone %in% kopgeig.zone.nums ~ 1,
      TRUE ~ 0
    ))
  
  message("\n✔ TALLYING CLIMATE MATCHES...")
  
  # Calculate total GPS records within 
  # target country/ies KG zones
  df_results = data_kg %>%
    dplyr::group_by(order, family, species) %>%
    dplyr::summarise(
      # Total GBIF records 
      total_n = n(), 
      # Total records in KG zones 
      total_records_in_kg = sum(kg_score),
      prop_records_in_kg = round( ( sum(kg_score)/n() ) * 100, digits = 3)
    )
  
  # add in summary info for each zone, dynamically
  zone_summary_list = list()
  
  for (num in kopgeig.zone.nums) {
    zone_summary = data_kg %>%
      group_by(order, family, species) %>%
      summarise(
        !!paste0("total_records_in_", num) := sum(kg_zone == num),
        !!paste0("prop_records_in_", num) := round(sum(kg_zone == num) / n() * 100, digits = 3),
        .groups = 'drop'
      )
    
    # Append the summary to the list
    zone_summary_list[[paste0("zone_", num)]] = zone_summary
  }#for
  
  
  for (zone_summary in zone_summary_list) {
    df_results = df_results %>%
      #left_join(zone_summary, by = "species")
      left_join(zone_summary, by = c("order", "family", "species"))
  }#for
  
  # Combine results with table with target country/ies records only 
  table =
    dplyr::left_join(df_results, target_records_summed, by = c("order", "family", "species")) 
  
  if(length(HR.iso.country.code) > 0){
  # now add on the high risk country records
    for(h in 1:length(HR.list)){
      table = 
        dplyr::left_join(table, HR.list[[h]], by = c("order", "family", "species")) 
    }#for
  }#if
  
  # replace all NAs with zeroes 
  table = table %>%
    dplyr::mutate_at(vars(contains("total_records_in_")), ~ replace_na(., 0))
  
  TABLE.LIST[[p]] = table
  
  message(paste0("\n✔ COMPLETED CHUNK ", p, ". OUTPUT TABLE HAS ", nrow(table),
                 " rows"))
  
}#for

#########################################################################
# Record the end time
#########################################################################
end_time = Sys.time()
#########################################################################
# Calculate the time taken
#########################################################################
message("TASK STARTED AT: \n", start_time, "\nTASK COMPLETED AT: \n",
        end_time)
#########################################################################

#########################################################################
# Save table of results to file 
#########################################################################

# Combine all the table outputs in the TABLE.LIST here:

message("\n✔ COMBINING ALL DATA INTO ONE OUTPUT FILE...")

# combine all the output DFs in TABLE.LIST into one DF
COMBO.DF = dplyr::bind_rows(TABLE.LIST)
#COMBO.DF = read.csv("final_sp_table_original.csv")

# group by species (sum up across all rows with the same species name)
COMBO.DF = COMBO.DF %>%
  dplyr::group_by(order, family, species) %>%
  dplyr::summarise(across(!starts_with("prop_records"), sum))

# create a copy for later
COMBO.DF.SYN = COMBO.DF

##########################################################
# FUNCTION TO GET KG PROPORTIONS 
calc_proportions = function(DF){

# recalculate proportions
total_cols = grep("^total_records_in_", names(DF), value = TRUE)
prop_names = gsub("total", "prop", total_cols)

options(scipen=999)

# calculate proportions in each KG zone
prop_df = DF %>%
  dplyr::ungroup() %>%  
  dplyr::mutate(across(all_of(total_cols), ~ round(./total_n * 100, 2))) %>%
  #dplyr::mutate(across(total_cols, ~ round(./total_n*100, 2))) %>%
  dplyr::rename_with(~prop_names, starts_with("total_records_in_")) %>%
  dplyr::select(-order, -family, -species, -total_n)

return(dplyr::bind_cols(DF, prop_df)) 
}
# FUNCTION TO GET KG PROPORTIONS 
##########################################################

message("\n✔ CALCULATING PROPORTIONS PER KG ZONE...")

COMBO.DF = calc_proportions(COMBO.DF)

write.csv(COMBO.DF, "OUTPUTS/GBIF_DATA/WATCHLIST_FULL.csv", row.names = FALSE)

message("\n✔ FULL WATCHLIST OUTPUT FILE WRITTEN")

########################################################
# COLLATE SYNONYMS
########################################################

message("\n✔ COLLATING TAXONOMIC SYNONYMS...")

# read in the original input list of spp so that we can look for synonyms
ORIGINAL.INPUT = read.csv("OUTPUTS/FILTERED_SYNONYMS_INC_INPUT_DATA.csv")
# keep just the important columns
ORIGINAL.INPUT.selected = dplyr::select(ORIGINAL.INPUT,
                                        c(status, species, 
                                          original.input.sp))

# replace species names with their synonym
for(p in 1:nrow(COMBO.DF.SYN)){
  
  synonyms = ORIGINAL.INPUT.selected %>%
    dplyr::filter(original.input.sp == COMBO.DF.SYN$species[p]) 
  
  COMBO.DF.SYN = COMBO.DF.SYN %>%
    dplyr::mutate(species = if_else(species %in% synonyms$species, 
                                    COMBO.DF.SYN$species[p], species))
  
}#for

# group by species and sum up
# COMBO.DF contains all species, including synonyms
# COMBO.DF.SYN contains less, since synonyms were merged
COMBO.DF.SYN = COMBO.DF.SYN %>%
  group_by(order, family, species) %>%
  summarize(across(everything(), sum))

message("\n✔ CALCULATING PROPORTIONS PER KG ZONE...")

COMBO.DF.SYN = calc_proportions(COMBO.DF.SYN)

# do some checks
# COMBO.DF.SYN %>%
#   dplyr::filter(species == "Acalypha poiretii") %>%
#   dplyr::pull(prop_records_in_kg)

write.csv(COMBO.DF.SYN, "OUTPUTS/GBIF_DATA/WATCHLIST_SYN_COLLATED.csv", 
          row.names = FALSE)

message("\n✔✔ WATCHLIST OUTPUT FILE WITH COLLATED SYNONYMS WRITTEN ✔✔")
