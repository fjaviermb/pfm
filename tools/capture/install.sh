#!/bin/bash

if [ -z "$TOOLS_DIR" ]; then
	echo "TOOLS_DIR variable is empty"
	exit -1;
else

	if [ ! -d "$TOOLS_DIR" ]; then
		echo "TOOLS_DIR variable is not a valid directory ["$TOOLS_DIR"]"
		exit -1;
	fi 
fi

CAPTURE_DIR=$TOOLS_DIR"/capture"
BIN_DIR="/usr/local/bin"

function createLink()
{
	script=$0
	source=$CAPTURE_DIR"/"$1
	destination=$BIN_DIR"/"$1
	if [ -f $source ]; then

		if [ -f $destination ]; then
			echo "Warning, link already exists. Removing current link...["$destination"]"
			rm "$destination"
		fi		

		echo "Creating link ["$destination"] from ["$source"]"
		ln -s $source $destination
	else
		echo "Error, source file doesn't exists. Source ["$source"]"
		exit -1;
	fi
}

createLink pcap_tcpdump_extended.sh
createLink pcap_tcpdump.sh
createLink pcap_tcpick.sh
createLink pcap_generate_week.sh
createLink pcap_generate_week.conf
createLink pcap_tshark_export.sh
createLink pcap_tshark_export_stream.sh
createLink pcap_scapy_generate_week.sh
createLink pcap_scapy_generate_week.py
createLink pmaster_generate.py
createLink pmaster_generate.sh


