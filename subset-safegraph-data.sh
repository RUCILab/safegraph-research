#!/bin/bash

# This is a script to subset SafeGraph weekly summaries into a particular state.
# USAGE: ./subset-safegraph-data.sh STATE
# A two-digit US state code is expected as an argument

# Subsets the Safegraph data into NJ-only
state=$1

for i in *.csv.gz; do
    # Strip out the extensions from the filenames
    echo "Subsetting $i for $state"
    filename=$(echo "$i" | cut -c 1-26)

    # Add the state name to the new filename
    subset_filename="${state} ${filename}"

    # Use zgrep to filter each file
    zgrep "$state" "$i" > "$subset_filename".csv
done


