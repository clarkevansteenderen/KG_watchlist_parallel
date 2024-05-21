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

# Define the main directory
main_dir = "RUNS/"

# Find all subdirectories
# the [-1] removes the root directory, RUNS/ from the list
subdirs = list.dirs(main_dir, recursive = TRUE, full.names = TRUE)[-1]

# List to store dataframes
df_list = c()
logfile_list = c()

# Loop through each subdirectory
for (q in subdirs) {
  # Construct the full path to the target file
  file_path_table = file.path(q, paste0("results/summary/", output.file.name))
  file_path_log = file.path(q, "results/summary/SKIPPED_SP_LOGFILE.txt")
  
  # Check if the file exists for the output table
  if (file.exists(file_path_table)) {
    # Read the Excel file and append it to the list
    df = read.csv(file_path_table)
    df_list = dplyr::bind_rows(df_list, df)
  }
  
  # Check if the file exists for the logs
  if (file.exists(file_path_log)) {
    # Read the text file and append it to the list
    logs = read.csv(file_path_log, header = FALSE)
    logfile_list = dplyr::bind_rows(logfile_list, logs)
  }
  
} # for

# write combined dataframes from all parallel runs to the proj directory
write.csv(df_list, "watchlist_final.csv")
write.csv(logfile_list, "watchlist_logfile_final.csv")
