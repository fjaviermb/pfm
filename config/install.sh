#!/bin/bash

source ./global.conf

if [ -z "$CONFIG_DIR" ]; then
	echo "CONFIG_DIR variable is empty"
	exit -1;
else

	if [ ! -d "$CONFIG_DIR" ]; then
		echo "CONFIG_DIR variable is not a valid directory ["$TOOLS_DIR"]"
		exit -1;
	fi 
fi

BIN_DIR="/usr/local/bin"

function createLink()
{
	script=$0
	source=$CONFIG_DIR"/"$1
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

createLink global.conf

