##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This is the meat of the program

# (1) If available, downloads the data available on GBIF for that species, and
# saves it in the OUTPUTS/GBIF_DATA/ folder

# (2) The data folder is read back into the R environment and filtered 
# (duplicates removed). A tally is kept of species already in the target country.
# The GPS locality of each record in the data file is assigned to a 
# Koppen-Geiger climate zone by number (e.g. Af = 1, Am = 2). These numbers are
# then matched to the KG zones in the target country. If they are shared, a "1"
# is assigned. If not, a "0" is recorded.

# (5) The final watchlist file is written to the GBIF_DATA folder

##################################################################

source("KG_run_setup.R")

#########################################################################
# Record the start time
#########################################################################

start_time = Sys.time()

#########################################################################
    
message(paste0("DOWNLOAD STARTED AT ", Sys.time()))
    
# Download records for the taxon keys

gbif_download = rgbif::occ_download(
  rgbif::pred_in("speciesKey", species_keys),
  format = "SIMPLE_CSV",
  user = user.input$gbif.username[1],
  pwd = user.input$gbif.password[1],
  email = user.input$gbif.email[1]
)

message("\nDOWNLOADING FROM GBIF...")

rgbif::occ_download_wait(gbif_download, 
                         quiet = FALSE, 
                         status_ping = 3) 
    
result = rgbif::occ_download_get(key = gbif_download, 
         overwrite = TRUE, path = paste0("OUTPUTS/GBIF_DATA/"))
    
# check the size of the downloaded folder -> convert from bytes to MB
result.filesize = round(file.size(result)/1000000, 2)

message(paste0("\nREADING IN GBIF DOWNLOAD... ", result.filesize, " MB"))

import_back_file = rgbif::occ_download_import(result)

##########################################################################
# Are any records already known from the target country?
##########################################################################

message("CHECKING FOR RECORDS ALREADY IN ", target.country)

# Set aside records from target country/ies for later 
target_records = import_back_file %>%
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
  dplyr::filter(country %in% iso.country.code)

# Calculate if any records are already in target country/ies
target_records_2 = target_records %>%
  dplyr::group_by(species, .drop = FALSE) %>%
  dplyr::summarise(
    n_records_in_target_countries = n()
  )

##########################################################################
# Koppen-Geiger analysis for global records 
##########################################################################

# Keep all other records not in target country/ies
df = import_back_file %>%
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
  dplyr::filter(!country %in% iso.country.code )

# Drop rows with no GPS data -> this can result in columns with NAs.
df = df %>% 
  tidyr::drop_na(lat, lon)

# Remove any duplicate GPS points 
df = df %>%
  dplyr::distinct(lat, lon, .keep_all = TRUE)

# make sure that lat and lon are numeric - some GBIF files have one or both
# as characters for some odd reason!
df$lat = as.numeric(df$lat)
df$lon = as.numeric(df$lon)

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

# Classify if each GPS point lies in a KG zone present 
# in the target country/ies
# these are specified in the INPUT.csv file edited by the user
data_kg = data %>%
  dplyr::mutate(kg_score = dplyr::case_when(
    kg_zone %in% kopgeig.zone.nums ~ 1,
    TRUE ~ 0
  ))

# Calculate proportion of GPS records within 
# target country/ies KG zones
df_results = data_kg %>%
  dplyr::group_by(species) %>%
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
    group_by(species) %>%
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
    left_join(zone_summary, by = "species")
}#for

# Combine results with table with target country/ies records only 
table =
  dplyr::left_join(df_results, target_records_2, by = c("species")) %>%
  tidyr::replace_na(list(n_records_in_target_countries = 0))

#########################################################################
# Record the end time
#########################################################################

end_time = Sys.time()

#########################################################################
# Calculate the time taken
#########################################################################

time_taken = round(end_time - start_time, 2)

#########################################################################
# report any possible issues as the loop progresses
#########################################################################

#########################################################################

message(paste0("Download completed in ", round(time_taken, 2), " minutes"))

#########################################################################
# Save table of results to file 
#########################################################################

output_file_path = paste0(paste0("OUTPUTS/GBIF_DATA/"), output_file_name)
write_csv(table, output_file_path)

message(paste0("\nOutput file written to ", output_file_path))

