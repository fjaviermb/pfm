#!/bin/bash

source $(dirname "$0")"/pcap_generate_week.conf"

# CONSTANTS
MASTER_ATTACK_FILE="master-listfile-condensed.txt"
MASTER_ATTACK_FILE_OUTPUT="master-listfile-condensed.csv"

# PCAP options
OPTIONS=""
SEPARATOR=","

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
   -s|--separator)
   	    OPTIONS="$OPTIONS -s $2"
	    SEPARATOR=$2
    	shift
    	shift
    	;;
    *)  
        echo "Invalid argument ["$1"]"
        echo "Usage:  TODO"
        exit -1;
        ;;
    esac
done


# Calculate filename input, just for information
FILENAME=$MASTER_ATTACK_FILE 
FILE_DIR=$DATASETS_DIR
INPUT_FILE=$FILE_DIR"/"$FILENAME

OUTPUT_FILENAME=$RAW_DIR"/"$MASTER_ATTACK_FILE_OUTPUT


echo DATASET_BASE_DIR  	= "${DATASETS_DIR}"
echo INPUT_FILE		= "${INPUT_FILE}"
echo OUTPUT_FILE	= "${OUTPUT_FILENAME}"

OPTIONS=$OPTIONS" -r "$RAW_DIR
OPTIONS=$OPTIONS" -a "$DATASETS_DIR

python /usr/local/bin/pmaster_generate.py $OPTIONS > $OUTPUT_FILENAME

