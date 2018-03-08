


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

OUTPUT_FILENAME=$RAW_DIR"/"$YEAR"_"$MODE"_week"$WEEK"_"$DAY"_"$TYPE".csv"

if [ $QUOTED -eq "0" ] 
then
	OUTPUT_PREFIX=""$YEAR"$SEPARATOR"$MODE"$SEPARATOR""week"$WEEK"$SEPARATOR"$DAY"$SEPARATOR"$TYPE""

	if [ $TEST -eq "0" ] 
	then
		zcat "${INPUT_FILE}" | /usr/local/bin/pcap_tshark_export_stream.sh $OPTIONS | grep -E  "^(1,17|17|1|6)" |  sed -e "s/^/${OUTPUT_PREFIX}${SEPARATOR}/" > $OUTPUT_FILENAME
	else
		zcat "${INPUT_FILE}" | /usr/local/bin/pcap_tshark_export_stream.sh $OPTIONS | grep -E  "^(1,17|17|1|6)" |  sed -e "s/^/${OUTPUT_PREFIX}${SEPARATOR}/" | head
	fi
else	
	OUTPUT_PREFIX="\""$YEAR"\"$SEPARATOR\""$MODE"\"$SEPARATOR\"week"$WEEK"\"$SEPARATOR\""$DAY"\"$SEPARATOR\""$TYPE"\""

        if [ $TEST -eq "0" ]
        then
		zcat "${INPUT_FILE}" | /usr/local/bin/pcap_tshark_export_stream.sh $OPTIONS | grep -E  "^(\"1,17\"|\"17\"|\"1\"|\"6\")" |  sed -e "s/^/${OUTPUT_PREFIX}${SEPARATOR}/" > $OUTPUT_FILENAME
	else
	        zcat "${INPUT_FILE}" | /usr/local/bin/pcap_tshark_export_stream.sh $OPTIONS | grep -E  "^(\"1,17\"|\"17\"|\"1\"|\"6\")" |  sed -e "s/^/${OUTPUT_PREFIX}${SEPARATOR}/" | head
	fi
fi


#zcat "${INPUT_FILE}" | sudo /usr/local/bin/pcap_tshark_export_stream.sh | grep -e  "^\"17\"|^\"6\"|^\"1\"" |  sed -e "s/^/${OUTPUT_PREFIX},/"
