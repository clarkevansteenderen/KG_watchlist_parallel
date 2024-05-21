#########################################################################
# SETUP
#########################################################################

library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)

# change the divide.dataset.into value from 48 if you add more email addresses
# also then edit the usernames and email addresses

################################################################################
# GENERATE THE SPECIES LIST
################################################################################

# read in the input file with user-changed parameters
input.params = read.delim("WATCHLIST_INPUT_FILE.txt", header = FALSE)
colnames(input.params) = c("parameter", "choice")
rownames(input.params) = input.params$parameter
input.params = dplyr::select(input.params, !parameter)

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

koppengeiger.raster.path = filter(input.params,
                                  row.names(input.params) %in% 
                                  c("KOPPEN-GEIGER RASTER PATH"))$choice

###############################################################################
# GENERATE CUSTOMISED FILES WITH INPUT PARAMETERS FOR EACH RUN
###############################################################################

# extract more info from the user's input file

iso.country.code = filter(input.params,
                          row.names(input.params) %in% 
                          c("ISO COUNTRY CODE"))$choice

spp.name.column = filter(input.params,
                         row.names(input.params) %in% 
                          c("SPECIES NAME COLUMN"))$choice

koppengeiger.zones = filter(input.params,
                            row.names(input.params) %in% 
                            c("KOPPEN-GEIGER ZONES"))$choice 
koppengeiger.zones = as.numeric(unlist(strsplit(koppengeiger.zones, ",\\s*")))

output.file.name = filter(input.params,
                          row.names(input.params) %in% 
                          c("OUTPUT FILE NAME"))$choice

keep.downloads = filter(input.params,
                        row.names(input.params) %in% 
                        c("KEEP DOWNLOADS"))$choice

num.spp = filter(input.params,
                 row.names(input.params) %in% 
                  c("NUMBER OF SPECIES TO PROCESS"))$choice

#########################################################################
# GENERATE SPECIES LIST 
#########################################################################

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

#########################################################################
# DIVIDE SPECIES LIST INTO SUBSETS - HERE IT IS SET TO 48 
# BASED ON THE NUMBER OF EMAIL ADDRESSES AT OUR DISPOSAL
#########################################################################

# Divide the griis.other dataset into subsets
griis.rows = nrow(griis.other.filtered)

# if the dataset is small -> less than 48 spp, then don't divide it up into
# subsets at all
if(griis.rows < 48){
  message("There are fewer than 48 species in the list")
  divide.dataset.into = 1
}# if

if(griis.rows >= 48){
divide.dataset.into = 48 # there are 16 email addresses for this, each can 
# run 3 simultaneous downloads -> total of 48 possible at a time
}#else

remainder = griis.rows %% divide.dataset.into
base_size = griis.rows %/% divide.dataset.into

# Adjust the sizes to account for the remainder
sizes = rep(base_size, divide.dataset.into)

# if there is a remainder, add that to the last subset dataframe size to avoid
# losing them!
if(remainder > 0){
  sizes[divide.dataset.into] = sizes[divide.dataset.into] + remainder
}

# create indices for each subset of the data
indices = list()
start = 1

for (i in 1:length(sizes)) {
  end <- start + sizes[i] - 1
  indices[[i]] <- start:end
  start <- end + 1
}

data.subsets = list()

for(p in 1:divide.dataset.into){
  data.subsets[[p]] = griis.other.filtered[indices[[p]], ]
}

#View(as.data.frame(data.subsets[48]))

# create folders for each subset (here, divided into 48)

if(!dir.exists("RUNS")){
  dir.create("RUNS")
}

dirnums = seq(1:divide.dataset.into)

for(p in dirnums){
  if (!dir.exists(paste0("RUNS/RUN", p))) {
    # create folder
    dir.create(paste0("RUNS/RUN", p))
    # write the relevant df to it 
    write.csv(data.subsets[[p]], 
              paste0("RUNS/RUN", p, "/watchlist_subset_", p, ".csv"),
              row.names = FALSE)
  }
}

#########################################################################
# GENERATE CUSTOMISED INPUT.CSV FILES FOR EACH INDIVIDUAL RUN FOLDER
#########################################################################

input.file.template = data.frame(matrix(ncol = 10))
colnames(input.file.template) = c("spp.file.path", "spp.name.column", "num.spp", 
                                  "gbif.username", "gbif.password", "gbif.email",
                                  "iso.country.code", "koppengeiger.zone.numbers",
                                  "output.file.name", "keep.downloads")

##############################################################################

# Determine the number of rows needed based on the longest vector ->
# country codes or koppen zones could have multiple entries
num_rows <- max(length(iso.country.code), length(koppengeiger.zones))

# dynamically set the number of rows to the number of koppengeiger zones
input.file.template <- input.file.template[rep(1, num_rows), ]
rownames(input.file.template) = seq_len(nrow(input.file.template))

##############################################################################

# Now set the constant variables that will be the same for all files
# Assign the variables to the columns, ensuring proper lengths

if (length(iso.country.code) == 1) {
  input.file.template$iso.country.code[1] <- iso.country.code
} else {
  input.file.template$iso.country.code <- iso.country.code
}

if (length(spp.name.column) == 1) {
  input.file.template$spp.name.column[1] <- spp.name.column
} else {
  input.file.template$spp.name.column <- spp.name.column
}

if (length(num.spp) == 1) {
  input.file.template$num.spp[1] <- num.spp
} else {
  input.file.template$num.spp <- num.spp
}

if (length(output.file.name) == 1) {
  input.file.template$output.file.name[1] <- output.file.name
} else {
  input.file.template$output.file.name <- output.file.name
}

if (length(keep.downloads) == 1) {
  input.file.template$keep.downloads[1] <- keep.downloads
} else {
  input.file.template$keep.downloads <- keep.downloads
}

if (length(koppengeiger.zones) == 1) {
  input.file.template$koppengeiger.zone.numbers[1] <- koppengeiger.zones
} else {
  input.file.template$koppengeiger.zone.numbers <- koppengeiger.zones
}

##############################################################################

# each email address can be used for three simultaneous downloads,
# so each address here can be the credentials for three separate RUN folders/jobs
email.addresses = c("watchlist01@gmx.com", "watchlist02@gmx.com",
                    "watchlist03@gmx.com", "watchlist04@gmx.com",
                    "watchlist05@gmx.com", "watchlist06@proton.me",
                    "watchlist07@proton.me", "watchlist08@proton.me",
                    "watchlist09@proton.me", "watchlist010@proton.me",
                    "watchlist011@proton.me", "watchlist012@proton.me",
                    "watchlist013@proton.me", "watchlist014@proton.me",
                    "clarke.vansteenderen@ru.ac.za", "vsteenderen@gmail.com")

# create three reps of each so that you can use this in a loop later
email.addresses.reps = c()

for(q in 1:length(email.addresses)){
  email.addresses.reps = c(email.addresses.reps, rep(email.addresses[q], 3))
}


gbif.usernames = c("watchlist01", "watchlist02", "watchlist03", "watchlist04",
                   "watchlist05", "watchlist06", "watchlist07", "watchlist08",
                   "watchlist09", "watchlist010", "watchlist011", "watchlist012",
                   "watchlist013", "watchlist014",
                   "vsteenderen", "clarke.vansteenderen")

# do the same for the usernames 
usernames.reps = c()
for(q in 1:length(gbif.usernames)){
  usernames.reps = c(usernames.reps, rep(gbif.usernames[q], 3))
}

# edit the number here for Watchlist001# accordingly if you add more
# email addresses
gbif.passwords = c(rep("Watchlist001#",14), rep("roxie2@!",2))

# and the same for passwords
passwords.reps = c()
for(q in 1:length(gbif.usernames)){
  passwords.reps = c(passwords.reps, rep(gbif.passwords[q], 3))
}

##############################################################################
# now loop through each folder to create a customised INPUT file, with the
# correct gbif username and password
##############################################################################

for(t in dirnums){
  # create a copy of the template
  INPUT = input.file.template
  # set the remaining columns
  INPUT$spp.file.path[1] = paste0("RUNS/RUN", t, "/watchlist_subset_", t, ".csv")
  INPUT$gbif.username[1] = usernames.reps[t]
  INPUT$gbif.password[1] = passwords.reps[t]
  INPUT$gbif.email[1] = email.addresses.reps[t]
  # remove NAs
  INPUT[is.na(INPUT)] = ""
  
  # write the INPUT file to each RUN folder
  write.csv(INPUT, 
            paste0("RUNS/RUN", t, "/INPUT.csv"),
            row.names = FALSE)
}# for

##############################################################################
