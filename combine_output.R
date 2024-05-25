##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# Each RUNS/RUN(n)/results/summary folder contains the final output table,
# and the log file for the run on each of the 48 subsets. This script fetches
# these output files from each of the 48 folders, and combines them back into
# one. The resulting species watchlist CSV file, and log file are written to the
# root folder as:

# "watchlist_final.csv" and
# "watchlist_logfile_final.csv"

##################################################################

library(dplyr)
library(gtools)

# read in the input file with user-changed parameters
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

output.file.name = filter(input.params,
                          row.names(input.params) %in% 
                            c("OUTPUT FILE NAME"))$choice

# Define the main directory
main_dir = "RUNS/"

# Find all subdirectories
# the [-1] removes the root directory, RUNS/ from the list
subdirs = list.dirs(main_dir, recursive = TRUE, full.names = TRUE)[-1]
# keep just the dirs that contain "results/summary" in the path
subdirs = subdirs %>%
  grep("results/summary", ., value = TRUE) 
# order them from 1 to n, rather than 1,10,11, etc.
subdirs = gtools::mixedsort(subdirs)

# get the full file path for each output file
filepaths.list = paste(subdirs, output.file.name, sep="/")
logfile.list = paste(subdirs, "SKIPPED_SP_LOGFILE.txt", sep="/")

# List to store dataframes
df_list = c()
logfile_list = c()

##################################################################
##                Loop through all dirs and bind dfs            ##
##################################################################
ind = 1
for (q in subdirs) {
  # Construct the full path to the target file
  file_path_table = file.path(q, output.file.name)
  file_path_log = file.path(sub("/results/.*", "", q), paste0("RUN", ind, ".out"))
  
  #Check if the file exists for the output table
  if (file.exists(file_path_table)) {
    # Read the Excel file and append it to the list
    df = read.csv(file_path_table)
    df_list = rbind(df_list, df)
  }
  
  # Check if the file exists for the logs
  if (file.exists(file_path_log)) {
    # Read the text file and append it to the list
    logs = readLines(file_path_log)
    logfile_list = c(logfile_list, logs)
  }
  
  ind = ind +1
  
} # for

##################################################################
##                         Find skipped species                 ##
##################################################################

original.input = read.csv("griis_data/griis_filtered_database.csv")

# find which row numbers have NA
missing.rows = which(is.na(df_list$species))
# get the species names for these rows
missing.species = original.input[missing.rows,] %>%
  dplyr::arrange(., accepted_name.species)

write.csv(missing.species, "MISSING_SPECIES.csv", row.names = FALSE)
message(paste0("MISSING SPECIES FILE WRITTEN TO: ", 
               getwd(), "/MISSING_SPECIES.csv"))

##################################################################
##                         Write output files to wd             ##
##################################################################

# now that we know where the NA rows are, and have saved the missing species,
# we can delete the NA rows from the final dataset
df_list = na.omit(df_list)

# I've noticed that the total_records_in_kg does not always equal the sum of the
# total_records_in_ each KG zone. This seems to be due to taxonomic synonyms
# Here, let's add a column to mark where these issues have arisen

check.df = dplyr::select(df_list, species, contains("total_records_in"),
                         -c(total_records_in_kg, species))

# sum across cols
check.df = check.df %>%
  rowwise() %>%
  mutate(sum = sum(c_across(everything()))) 


df_list$sum = check.df$sum

# create a new column with TRUE or FALSE if the total_records_in_kg tallies
# with the sum across each KG zone
df_list = df_list %>%
  mutate(sum_equals_total_records_in_kg = sum == total_records_in_kg)

df_list_out = dplyr::filter(df_list, 
                            sum_equals_total_records_in_kg == "TRUE") %>%
  dplyr::select(., -c(sum_equals_total_records_in_kg, sum)) %>%
  dplyr::arrange(., species) # order alphabetically -> this will help spot 
# duplicates

df_list_tax_issues = dplyr::filter(df_list, 
                                   sum_equals_total_records_in_kg == "FALSE") %>%
  dplyr::select(., -c(sum_equals_total_records_in_kg, sum)) %>%
  dplyr::arrange(., species)

# this fetches the species with issues from the original file as well, so that
# we can also see the other original column info for these spp
false.rows = which(df_list$sum_equals_total_records_in_kg == "FALSE")

df_list_tax_issues_b = original.input[false.rows,] %>%
  dplyr::arrange(., accepted_name.species)

# write final combined dataframes from all parallel runs to the proj directory
write.csv(df_list_out, "WATCHLIST_OUTPUT.csv", row.names = FALSE)
message(paste0("FINAL WATCHLIST FILE WRITTEN TO: ", getwd(), "/WATCHLIST_OUTPUT.csv"))

# write dataframes with taxonomic issues to the proj directory
write.csv(df_list_tax_issues, "GBIF_TAXONOMIC_ISSUES_A.csv", row.names = FALSE)
message(paste0("GBIF TAXONOMIC ISSUES FILE A WRITTEN TO: ", 
               getwd(), "/GBIF_TAXONOMIC_ISSUES_A.csv"))

write.csv(df_list_tax_issues_b, "GBIF_TAXONOMIC_ISSUES_B.csv", row.names = FALSE)
message(paste0("GBIF TAXONOMIC ISSUES FILE B WRITTEN TO: ", 
               getwd(), "/GBIF_TAXONOMIC_ISSUES_B.csv"))

# write combined log file
writeLines(logfile_list, "WATCHLIST_LOG_OUTPUT.txt")
message(paste0("FINAL LOG FILE WRITTEN TO: ", getwd(), 
               "/WATCHLIST_LOG_OUTPUT.txt"))

##################################################################
##                  Get some summary stats                      ##
##################################################################

# for df_list_out:

summaries <- df_list_out %>%
  summarise(across(c(total_n, mins, size.mb), list(
    mean = ~mean(., na.rm = TRUE), 
    sd = ~sd(., na.rm = TRUE), 
    min = min, 
    max = max, 
    median = ~median(., na.rm = TRUE),
    sum = sum
  ))) 
