##################################################################
##                         CODE RUNDOWN                         ##
##################################################################

# (1) The new input data is divided into n subsets (since there are n email
# addresses available, and each can download a maximum of three records at the
# same time from GBIF: n * 3 = 3n)

# (2) A folder called RUNS is created, into which n empty folders are written
# and named RUN1 up to RUN48(n) (i.e. RUNS/RUN1, RUNS/RUN2, etc.)
# Each of the n species subsets is then written into each corresponding
# RUN(n) folder. I.e. RUNS/RUN1/watchlist_subset_1.csv, 
# RUNS/RUN2/watchlist_subset_2.csv, and so on.

# (3) Each RUNS/RUN(n) folder then also needs an INPUT.csv file, which contains 
# input parameters that have been taken from the WATCHLIST_INPUT_FILE.txt file,
# and GBIF account information (username, email address, and password).
# This is done so that every three consecutive data subsets get the same GBIF 
# user details

# If more email addresses and associated GBIF accounts are added to these n,
# then add them to the email_addresses.csv file so that the filtered dataset is divided
# into more subsets (e.g. 20 email addresses means that the data can be divided
# into subsets of 60 (20 x 3 downloads per user), rather than 51). Also add
# to the vectors containing GBIF usernames, passwords, and email addresses

#########################################################################
##        DIVIDE SPECIES LIST INTO n SUBSETS -                         ##
##         BASED ON THE NUMBER OF EMAIL ADDRESSES AT OUR DISPOSAL      ##
#########################################################################

gbif.info = read.csv("email_addresses.csv")
DIVISION.VAL = length(gbif.info$email.address) * 3

message(paste0("Read in ", nrow(gbif.info), " email addresses"))

# Divide the input data into subsets
synonym.rows = nrow(SYNONYM.LIST)

message(paste0("Dividing data (n = ", synonym.rows, ") into ", DIVISION.VAL, " subsets"))

# if the dataset is small -> less than n spp, then don't divide it up into
# subsets at all
if(synonym.rows < DIVISION.VAL){
  message(paste0("There are fewer than ", 
                 DIVISION.VAL, " species in the list. Subsetting not necessary."))
  divide.dataset.into = 1
}# if

if(synonym.rows >= DIVISION.VAL){
divide.dataset.into = DIVISION.VAL # there are 17 email addresses for this, each can 
# run 3 simultaneous downloads -> total of 51 possible at a time
}#else

remainder = synonym.rows %% divide.dataset.into
base_size = synonym.rows %/% divide.dataset.into

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
  data.subsets[[p]] = SYNONYM.LIST[indices[[p]], ]
}

#View(as.data.frame(data.subsets[51]))

# create folders for each subset (here, divided into 51)

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
## GENERATE CUSTOMISED INPUT.CSV FILES FOR EACH INDIVIDUAL RUN FOLDER  ##
#########################################################################

input.file.template = data.frame(matrix(ncol = 9))
colnames(input.file.template) = c("spp.file.path", "num.spp", 
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

##################################################################
##                    SET UP EMAIL ADDRESSES                    ##
##################################################################

# each email address can be used for three simultaneous downloads,
# so each address here can be the credentials for three separate RUN folders/jobs
gbif.emailaddresses = gbif.info$email.address

gbif.emailaddresses.reps = c()

# make three copies of each email address so that each account can be used to
# run 3 concurrent downloads. Do the same for usernames and passwords
for(q in 1:length(gbif.emailaddresses)){
  gbif.emailaddresses.reps = c(gbif.emailaddresses.reps, rep(gbif.emailaddresses[q], 3))
}

##################################################################
##                    SET UP USER NAMES                         ##
##################################################################

gbif.usernames = gbif.info$gbif.name

gbif.usernames.reps = c()

for(q in 1:length(gbif.usernames)){
  gbif.usernames.reps = c(gbif.usernames.reps, rep(gbif.usernames[q], 3))
}

##################################################################
##                    SET UP PASSWORDS                          ##
##################################################################

gbif.passwords = gbif.info$gbif.password
gbif.passwords.reps = c()

for(q in 1:length(gbif.passwords)){
  gbif.passwords.reps = c(gbif.passwords.reps, rep(gbif.passwords[q], 3))
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
  INPUT$gbif.username[1] = gbif.usernames.reps[t]
  INPUT$gbif.password[1] = gbif.passwords.reps[t]
  INPUT$gbif.email[1] = gbif.emailaddresses.reps[t]
  # remove NAs
  INPUT[is.na(INPUT)] = ""
  
  # write the INPUT file to each RUN folder
  write.csv(INPUT, 
            paste0("RUNS/RUN", t, "/INPUT.csv"),
            row.names = FALSE)
}# for

##############################################################################

message("Data prepared, and all the relevant folders written successfully :)")