#!/bin/bash

source $(dirname "$0")"/pcap_generate_week.conf"

# Default values
TYPE="inside"
YEAR="1999"
MODE="training"

# PCAP options
OPTIONS=""
SEPARATOR=","
TEST=0

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
    -q|--quoted)
	    # This option is not implemented and should be removed
	    QUOTED=1
    	OPTIONS="$OPTIONS -q"
    	shift
    	;;
    -w|--week)
    	WEEK="$2"
    	shift
    	shift
    	;;
    -m|--mode)
    	MODE="$2"
    	shift
    	shift
    	;;
    -d|--day)
    	DAY="$2"
    	shift
    	shift
    	;;
    -t|--type)
    	TYPE="$2"
    	shift
    	shift
    	;;
    -y|--year)
        YEAR="$2"
        shift
        shift
        ;;
    -test|--test)
	    TEST=1
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
FILENAME=$YEAR"_"$MODE"_week"$WEEK"_"$DAY"_"$TYPE".tcpdump.gz" 
FILE_DIR=$DATASETS_DIR"/week"$WEEK
INPUT_FILE=$FILE_DIR"/"$FILENAME

OUTPUT_FILENAME=$RAW_DIR"/"$YEAR"_"$MODE"_week"$WEEK"_"$DAY"_"$TYPE".csv"


echo DATASET_BASE_DIR  	= "${DATASETS_DIR}"
echo WEEK		= "${WEEK}"
echo MODE	     	= "${MODE}"
echo TYPE		= "${TYPE}"
echo DAY	    	= "${DAY}"
echo YEAR         	= "${YEAR}"
echo INPUT_FILE		= "${INPUT_FILE}"
echo OUTPUT_FILE	= "${OUTPUT_FILENAME}"

OPTIONS=$OPTIONS" -w "$WEEK
OPTIONS=$OPTIONS" -m "$MODE
OPTIONS=$OPTIONS" -t "$TYPE
OPTIONS=$OPTIONS" -d "$DAY
OPTIONS=$OPTIONS" -r "$RAW_DIR
OPTIONS=$OPTIONS" -a "$DATASETS_DIR

OUTPUT_PREFIX=""$YEAR"$SEPARATOR"$MODE"$SEPARATOR""week"$WEEK"$SEPARATOR"$DAY"$SEPARATOR"$TYPE""

python /usr/local/bin/pcap_scapy_generate_week.py $OPTIONS |  sed -e "s/^/${OUTPUT_PREFIX}${SEPARATOR}/" > $OUTPUT_FILENAME

