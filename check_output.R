##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# This script checks whether all RUNS/RUN(n) folders have completed,
# and have the output summary table in them.
# This can allow the user to decide whether they want to combine all their
# output with what is available, or to re-run the folders that did not
# complete

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
# order them from 1 to 48, rather than 1,10,11, etc.
subdirs = gtools::mixedsort(subdirs)

# get the full file path for each output file
filepaths.list = paste(subdirs, output.file.name, sep="/")
logfile.list = paste(subdirs, "SKIPPED_SP_LOGFILE.txt", sep="/")

# find whether all the files are there
num.files.present = length(which(file.exists(filepaths.list)) )

# find which subsets did not complete
not.completed = which(!file.exists(filepaths.list))

if(length(not.completed > 0)){
  message(paste0("\nThese subsets did not complete: ", 
                 paste(not.completed, collapse = " ")))
  
  message(paste0("\nRun this on the command line (or HPC):",
                 '\nfor p in ', paste(not.completed, collapse = " "), 
                 '; do nohup Rscript KG_run.R "${p}" &> "RUNS/RUN${p}/RUN${p}.out" & done'))
} #if

if(length(not.completed) == 0){
message("\n****ALL ", length(filepaths.list), " RUNS COMPLETED SUCCESSFULLY****\n")
}# if
