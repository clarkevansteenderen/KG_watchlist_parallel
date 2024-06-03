#!/bin/bash

############################################################################################
# SET THESE DIRECTORIES ACCORDINGLY
############################################################################################

# Change the base directory to the appropriate path
BASE_DIR="/mnt/lustre/users/cvansteenderen/kg_watchlist_V3/OUTPUTS/GBIF_DATA"
# Automatically detect the name of the zip file in the directory
ZIP_FILE=$(ls ${BASE_DIR}/*.zip | head -n 1)
UNZIP_DIR="${BASE_DIR}"
GBIF_HEADER="${BASE_DIR}/header.csv"
CHUNK_PREFIX="${BASE_DIR}/chunk_"
# Change the number of lines here to the appropriate number 
LINES_PER_FILE=4000000

############################################################################################
# Look for a zip folder in the specified directory, and if not found, exit the script
############################################################################################

if [ -z "$ZIP_FILE" ]; then
  echo "No zip file found in $BASE_DIR"
  exit 1
fi

############################################################################################
# Unzip the GBIF folder that was downloaded (huge file!)
############################################################################################

echo "Unzipping the downloaded GBIF folder $ZIP_FILE..."
unzip "$ZIP_FILE" -d "$UNZIP_DIR"
echo "Unzipping completed."

############################################################################################
# Look for the unzipped CSV file in the specified directory
############################################################################################

if [ -z "$INPUT_FILE" ]; then
  echo "No CSV file found in $BASE_DIR"
  exit 1
fi

############################################################################################
# Automatically detect the name of the CSV file in the directory
############################################################################################

INPUT_FILE=$(ls ${BASE_DIR}/*.csv | head -n 1)

############################################################################################
# Extract the header columns (first row) from the unzipped CSV file, and save it as
# header.csv 
############################################################################################

echo "Extracting header from the original file..."
head -n 1 "$INPUT_FILE" > "$GBIF_HEADER"
echo "Header extracted and saved to $GBIF_HEADER."

############################################################################################
# Split the original CSV file into chunks of a specified size (number of rows)
############################################################################################

echo "Splitting the original GBIF CSV file into chunks..."
awk -v lines_per_file="$LINES_PER_FILE" -v prefix="$CHUNK_PREFIX" 'NR==1 { next }
{ file=sprintf("%s%02d.csv", prefix, int((NR-2)/lines_per_file)+1); print > file }' "$INPUT_FILE"
echo "File split into chunks."

############################################################################################
# Add the header onto each chunk 
############################################################################################

echo "Appending the header to each chunk file..."
for file in ${CHUNK_PREFIX}*; do
  echo "Processing $file..."
  cat "$GBIF_HEADER" "$file" > "${file}.tmp" && mv "${file}.tmp" "$file"
done
echo "Header prepended to each chunk file."

############################################################################################
# Remove the double header now in the first file
############################################################################################

echo "Removing the header from the first chunk file..."
first_file=$(ls ${CHUNK_PREFIX}*.csv | head -n 1)
sed -i '1d' "$first_file"
echo "Header removed from $first_file."

echo "HOORAY - script completed successfully!"

############################################################################################
# Remove the header file
############################################################################################

rm "$GBIF_HEADER"

# Find the D drive path on your local PC
# df -h

# Change to the desired WD
# cd /mnt/d/Philip_Ivey/kg_watchlist_MULTI_automated_V3/tester

# run this to make the script executable:
# chmod +x split_gbif.sh

# to run the script:
# ./split_gbif.sh