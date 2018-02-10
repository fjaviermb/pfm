#!/bin/bash

source $(dirname "$0")"/pcap_generate_week.conf"

# Default values
TYPE="inside"
YEAR="1999"
MODE="training"

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
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
    *)  
    echo "Invalid argument ["$1"]"
    echo "Usage:  "
    exit -1;
    ;;
esac
done

echo DATASET_BASE_DIR  	= "${DATASET_BASE_DIR}"
echo WEEK		= "${WEEK}"
echo MODE	     	= "${MODE}"
echo TYPE		= "${TYPE}"
echo DAY	    	= "${YEAR}"
echo YEAR         	= "${DAY}"

FILENAME=$YEAR"_"$MODE"_week"$WEEK"_"$DAY"_"$TYPE".tcpdump.gz" 
FILE_DIR=$DATASETS_DIR"/week"$WEEK
INPUT_FILE=$FILE_DIR"/"$FILENAME
echo INPUT_FILE		= "${INPUT_FILE}"

OUTPUT_PREFIX=""$YEAR","$MODE",week"$WEEK","$DAY","$TYPE
OUTPUT_FILENAME=$RAW_DIR"/"$YEAR"_"$MODE"_week"$WEEK"_"$DAY"_"$TYPE".csv"
zcat "${INPUT_FILE}" | /usr/local/bin/pcap_tshark_export_stream.sh | grep -E  "^(\"1,17\"|\"17\"|\"1\"|\"6\")" |  sed -e "s/^/${OUTPUT_PREFIX},/" > $OUTPUT_FILENAME
#zcat "${INPUT_FILE}" | sudo /usr/local/bin/pcap_tshark_export_stream.sh | grep -e  "^\"17\"|^\"6\"|^\"1\"" |  sed -e "s/^/${OUTPUT_PREFIX},/"
