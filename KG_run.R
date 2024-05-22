##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This is the meat of the analysis, which kicks off a loop that goes through
# each species name in the input file (subsetted invasive species list).
# It then does the following:

# (1) If available, downloads the data available on GBIF for that species, and
# saves it temporarily in the data/zip folder

# (2) The data folder is read back into the R environment and filtered 
# (duplicates removed). A tally is kept of species already in the target country.
# The GPS locality of each record in the data file is assigned to a 
# Koppen-Geiger climate zone by number (e.g. Af = 1, Am = 2). These numbers are
# then matched to the KG zones in the target country. If they are shared, a "1"
# is assigned. If not, a "0" is recorded.

# (3) A summary table is dynamically generated as each species name is processed
# and scored, with totals and proportions of shared KG zones.

# (4) After each iteration of a species, the R global environment is cleared of
# large objects, and the zipped GBIF data folder is deleted from data/zip in 
# order to save space

# (5) Final output files (the watchlist itself and the log file) are written
# to RUNS/RUN(n)/data/summary 

# (6) The combine_output.R file is sourced to collate all outputs across the 48
# folders and store them as one output and one log file

##################################################################

source("KG_run_setup.R")

#########################################################################
# RUN THE LOOP
#########################################################################

for(p in 1:num_species_to_process) {
  
  tryCatch({
    
#########################################################################
# Record the start time
#########################################################################
    
start_time <- Sys.time()
    
#########################################################################
    
# Get species name
species_name <- species_names[p]
    
# print this progress message on the first and second iteration (Exclude time info, 
# as it's too early to estimate times)
    
message(paste0("\nRunning species ", 
species_name, ": ", p, " of ", 
num_species_to_process, " -> ", 
round(p/num_species_to_process*100, 2), "% complete."))
    
    gbif_taxon_keys <- tryCatch({
      
      rgbif::name_backbone_checklist(species_name) %>% 
        dplyr::filter(!matchType == "NONE") %>%
        dplyr::pull(usageKey)
    }, 
    error = function(e) {
      cat("Error occurred while fetching usageKey for", species_name, ":", 
          conditionMessage(e), "\n")
      return(NULL)  # Return NULL if usageKey is not found
    })
    
    # Check if usageKey is NULL, indicating an error occurred
    if (is.null(gbif_taxon_keys)) {
      error.log = c(error.log, p)
      # Move on to the next iteration rather than breaking the loop
      next
    }
    
    num.recs.available = rgbif::occ_count(scientificName = species_name)
    message(paste0("There are ", num.recs.available, " records on GBIF for ",
                   species_name, ". Usage key: ", gbif_taxon_keys))
    
    message(paste0("Download started at ", Sys.time()))
    
    # Check if the number of records is zero
    if (num.recs.available == 0) {
      error.log = c(error.log, p)
      message("Skipping due to zero records on GBIF")
      # Move on to the next iteration rather than breaking the loop
      next
    }
    
    # Download records for the taxon key
    gbif_download <- download_records(gbif_taxon_keys)
    
    rgbif::occ_download_wait(gbif_download) 
    
    result = rgbif::occ_download_get(key = gbif_download, overwrite = FALSE,
                                     path = paste0("RUNS/", arg, "/data/zip/"))
    
    
    # check the size of the downloaded folder -> convert from bytes to MB
    result.filesize = round(file.size(result)/1000000, 2)
    
    ############################################################################
    # decided to limit the file to only the first 500,000 rows of the data file
    # downloaded from GBIF. Doesn't make sense to skip it just because there
    # are too many records
    ############################################################################
    
    tryCatch({
      # limit the rows read in to 500,000
      import_back_file <- rgbif::occ_download_import(result, nrows = 500000)
    }, error = function(e) {
      # Code to handle the error
      print(paste("An error occurred in reading in the gbif folder:", e$message))
      return(NULL)
    })
    
    # Check if import_back_file is NULL, indicating an error occurred
    if (is.null(import_back_file)) {
      error.log = c(error.log, p)
      # Move on to the next iteration rather than breaking the loop
      next
    }
    
    # Check if import_back_file has zero rows, indicating a file format issue
    if (length(import_back_file) == 0) {
      error.log = c(error.log, p)
      message("The downloaded file has a formatting issue and will be skipped")
      # Move on to the next iteration rather than breaking the loop
      next
    } # if
    
    # ---------------------------------------------------------
    # Are any records already known from the target country/ies? 
    # ---------------------------------------------------------
    
    # Set aside records from target country/ies for later 
    target_records <- import_back_file %>%
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
    target_records_2 <- target_records %>%
      dplyr::group_by(species, .drop = FALSE) %>%
      dplyr::summarise(
        n_records_in_target_countries = n()
      )
    
    # ---------------------------------------------------------
    # Koppen-Geiger analysis for global records 
    # ---------------------------------------------------------
    
    # Keep all other records not in target country/ies
    df <- import_back_file %>%
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
    # deal with this in a try catch a bit further down
    df <- df %>% 
      tidyr::drop_na(lat, lon)
    
    # Remove any duplicate GPS points 
    df <- df %>%
      dplyr::distinct(lat, lon, .keep_all = TRUE)
    
    # make sure that lat and lon are numeric - some GBIF files have one or both
    # as characters for some odd reason!
    df$lat = as.numeric(df$lat)
    df$lon = as.numeric(df$lon)
    
    # Extract climate at these points 
    kg_extract <- terra::extract(
      x = kg_map,    # SpatRast containing climate and topo layers
      y = df[, c(7, 6)]  # SpatVect or data.frame containing GPS of study taxon (lon, lat)
    )
    
    # Join KG layer with GPS data 
    data <-
      dplyr::bind_cols(df, kg_extract) %>%
      dplyr::rename(
        kg_zone = Beck_KG_V1_present_0p0083
      ) %>%
      tidyr::drop_na(kg_zone) %>%
      dplyr::select(-c(ID))
    
    
    # Classify if each GPS point lies in a KG zone present 
    # in the target country/ies
    # these are specified in the INPUT.csv file edited by the user
    
    data_kg <- data %>%
      dplyr::mutate(kg_imp = dplyr::case_when(
        kg_zone %in% kopgeig.zone.nums ~ 1,
        TRUE ~ 0
      ))
    
    
    # Calculate proportion of GPS records within 
    # target country/ies KG zones
    df_results <- data_kg %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(
        # Total GBIF records 
        total_n = n(), 
        # Total records in KG zones 
        total_records_in_kg = sum(kg_imp),
        prop_records_in_kg = round( ( sum(kg_imp)/n() ) * 100, digits = 3)
      )
    
    # add in summary info for each zone, dynamically
    zone_summary_data = data.frame(matrix(ncol = length(kopgeig.zone.nums)*2, 
                                          nrow = 1))
    colnames(zone_summary_data) = climate_zone_names
    
    # Loop through each zone number
    for (num in kopgeig.zone.nums) {
      # Calculate total records for the current zone
      total_records <- sum(data_kg$kg_zone == num)
      # Calculate proportion of records for the current zone
      prop_records <- round((total_records / nrow(data_kg)) * 100, digits = 3)
      
      # Assign values to the respective columns in the summary data frame
      zone_summary_data[1, paste0("total_records_in_", num)] = total_records
      zone_summary_data[1, paste0("prop_records_in_", num)] = prop_records
    }
    
    # an issue that has come up is that if there are no GPS records, the lat and lon
    # columns are full of NAs. The df_results data frame is therefore empty at this point,
    # and the cbind below doesn't work
    
    tryCatch({
      df_results = cbind(df_results, zone_summary_data)
    }, error = function(e) {
      # Code to handle the error
      message(paste0("An error occurred, likely because ", species_name, 
                   " had missing GPS data. ", e$message))
      return(NULL)
    })
    
    # Check if df_results is NULL, indicating an error occurred
    if (is.null(df_results)) {
      error.log = c(error.log, p)
      # Move on to the next iteration rather than breaking the loop
      next
    }
    
    # Combine results with table with target country/ies records only 
    table <- 
      dplyr::left_join(df_results, target_records_2, by = c("species")) %>%
      tidyr::replace_na(list(n_records_in_target_countries = 0))
    
    super_table[p,] = table[1,]  
    
    # store the citation for each record
    gbif.citation = rgbif::gbif_citation(result)$download %>%
      unlist()
    
    super_table$citation[p] = str_extract(gbif.citation, "https://\\S+") 
    
  }, error = function(e) {
    # Print or log the error message
    cat("Error occurred at iteration", p, ":", conditionMessage(e), "\n")
    error.log = c(error.log, p)
    # Skip to the next iteration
    next
  })
  
  #########################################################################
  # Record the end time
  #########################################################################
  
  end_time <- Sys.time()
  
  #########################################################################
  # Calculate the time taken
  #########################################################################
  
  time_taken <- round(end_time - start_time, 2)
  
  super_table$mins[p] = as.numeric(time_taken)
  
  #########################################################################
  # get file size for the download
  #########################################################################
  
  super_table$size.mb[p] = result.filesize
  
  # keep writing to file in case something happens along the way
  temp_super_table = super_table %>%
    na.omit(.)
  write.csv(temp_super_table, paste0("RUNS/", arg, "/results/TEMP.super_table.csv"), 
            row.names = FALSE)
  
  #########################################################################
  # report any possible issues as the loop progresses
  #########################################################################
  
  error.log.sp.list = species_names[error.log]
  
  if(length(error.log) > 0){
    write.table(error.log.sp.list,
                file = paste0("RUNS/", arg, 
                              "/results/summary/SKIPPED_SP_LOGFILE.txt"), 
                col.names = F, row.names = F, 
                quote = F)
  }#if
  
  #########################################################################
  
  # to save space, remove these from the R environment after every iteration 
  # leave supertable though!
  
  rm(list = c("gbif_download",
              "result",
              "result.filesize",
              "import_back_file",
              "target_records",
              "target_records_2",
              "df",
              "kg_extract",
              "data",
              "data_kg",
              "df_results",
              "zone_summary_data",
              "table",
              "gbif.citation",
              "temp_super_table"))
  
  if(keep.folders == "n"){
  # delete the data folder in the data/zip/ dir to save space once all the
  # info has been extracted and stored in super_table
    unlink(paste0("RUNS/", arg, "/data/zip/*.zip"))
  }# if
  
} #for(p in 1:taxa_to_process)

#########################################################################
# END OF LOOP
#########################################################################

message("Complete")

#########################################################################
# Save table of results to file 
#########################################################################

output_file_path = paste0(paste0("RUNS/", arg, "/results/summary/"), output_file_name)
write_csv(super_table, output_file_path)

message(paste0("\nOutput file written to ", output_file_path))
