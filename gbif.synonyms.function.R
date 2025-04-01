######################################################################
# GET SYNONYMS FROM GBIF FUNCTION
######################################################################

gbif.synonyms = function(spp.list){
  
  SYNONYM.LIST = c()
  LOGFILE.LIST = c()
  
  # small subset to test
  #spp.list = dplyr::slice(spp.list, c(1:2))
  
  for(t in 1:nrow(spp.list)){
    
    synonyms.df = c()
    
    tryCatch({
      
      message(paste0("GETTING SYNONYMS FOR ", spp.list[t,], ": ", t, " OF ", 
                     nrow(spp.list)))
      
      ######################################################################
      #                        GET SYNONYMS HERE                           #
      ######################################################################
      
      # Here, I'm keeping only SPECIES entries (not HIGHERTAXON), and keeping
      # exact matches, in case there are spelling differences
      # I'm then removing any duplicate speciesKeys, and going with using
      # the speciesKey for downloading records rather than usageKey that I used
      # before
      
      synonyms.df = rgbif::name_backbone_checklist(spp.list[t,], verbose=T) %>%
        dplyr::select(usageKey, scientificName, status, matchType,
                      canonicalName, rank, species, speciesKey) %>%
        dplyr::filter(rank == "SPECIES", matchType %in% c("EXACT"), 
                      status == "ACCEPTED" | status == "SYNONYM") %>%
        # remove all duplicate speciesKey rows
        dplyr::distinct(speciesKey, .keep_all = TRUE)
    }, 
    error = function(e) {
      message(paste0("\nAn error occurred while fetching GBIF information for ", 
                     spp.list[t,], ": ", 
                     conditionMessage(e), " ...moving on\n"))
      
      return(NULL)  # Return NULL if usageKey is not found
    })#tryCatch
    
    # Check if usageKey is NULL, indicating an error occurred
    if (is.null(synonyms.df)) {
      LOGFILE.LIST = c(LOGFILE.LIST, spp.list[t,])
      # Move on to the next iteration rather than breaking the loop
      next
    }# if
    
    synonyms.df$num.recs = NA
    
    for(p in 1:nrow(synonyms.df)){
      synonyms.df$num.recs[p] = rgbif::occ_count(speciesKey = synonyms.df$speciesKey[p])
    }#for
    
    synonyms.df = synonyms.df %>% dplyr::filter(., num.recs != 0)
    
    synonyms.df$original.input.sp = spp.list[t,]
    
    SYNONYM.LIST = dplyr::bind_rows(SYNONYM.LIST, synonyms.df)
    
  }#for
  
  message(paste0("\nYOUR NEW SPECIES LIST CONTAINS: \n",
                 nrow(SYNONYM.LIST), " ENTRIES\n",
                 "YOUR ORIGINAL LIST CONTAINED:\n", nrow(spp.list),
                 " ENTRIES\n"))
  
  # return the full list of synonyms, and the logfile of the species not found
  return(list(SYNONYM.LIST = SYNONYM.LIST, LOGFILE.LIST = LOGFILE.LIST))
  
} #gbif.synonyms

