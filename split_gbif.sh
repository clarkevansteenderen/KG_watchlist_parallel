#!/bin/bash

############################################################################################
# Look for a zip folder in the specified directory, and if not found, exit the script
############################################################################################

if [ -z "$ZIP_FILE" ]; then
  echo "No zip file found in $BASE_DIR"
  exit 1
fi

############################################################################################
# Extract the CSV file name from the zip archive without fully extracting it to disk
############################################################################################

# Automatically detect the name of the CSV file inside the zip archive
INPUT_FILE=$(unzip -l "$ZIP_FILE" | awk '/\.csv$/ {print $4}' | head -n 1)

if [ -z "$INPUT_FILE" ]; then
  echo "No CSV file found in the zip archive"
  exit 1
fi

############################################################################################
# Extract the header columns (first row) from the zipped CSV file, and save it as header.csv
############################################################################################

echo "Extracting header from the zipped CSV file..."
unzip -p "$ZIP_FILE" "$INPUT_FILE" | head -n 1 > "$GBIF_HEADER"
echo "Header extracted and saved to $GBIF_HEADER."

############################################################################################
# Debugging: Print LINES_PER_FILE value
############################################################################################

echo "LINES_PER_FILE is set to: $LINES_PER_FILE"

############################################################################################
# Split the zipped CSV file into chunks without fully extracting it
############################################################################################

echo "Splitting the zipped CSV file into chunks..."
unzip -p "$ZIP_FILE" "$INPUT_FILE" | tail -n +2 | awk -v lines_per_file="$LINES_PER_FILE" -v CHUNK_PREFIX="$CHUNK_PREFIX" '
{
    file=sprintf("%s%02d.csv", CHUNK_PREFIX, int((NR-1)/lines_per_file)+1)
    print > file
}
'
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

# to run the script (this approach doesn't submit it as a qsub job, but rather runs straight from the console):
# ./split_gbif.sh