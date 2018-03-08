
DEFAULT_SEPARATOR=","
SEPARATOR=$DEFAULT_SEPARATOR
OPTIONS=""

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -s|--separator)
   	SEPARATOR="$2"
    	shift
    	shift
    	;;
    -q|--quoted)
    	OPTIONS="$OPTIONS -E quote=d"
    	shift
    	;;
    *)  
	    echo "Invalid argument ["$1"]"
    	echo "Usage:  "
    	exit -1;
    ;;
esac
done


OPTIONS="$OPTIONS -E separator=$SEPARATOR"

tshark -l -i - $OPTIONS -T fields -e ip.proto -e eth.dst -e eth.type -e frame.len -e eth.src -e icmp.checksum -e icmp.code -e icmp.type -e ip.checksum -e ip.dst -e ip.fragment -e ip.frag_offset -e ip.hdr_len -e ip.len -e ip.src -e ip.tos -e ip.ttl -e tcp.ack -e tcp.checksum -e tcp.dstport -e tcp.flags -e tcp.hdr_len -e tcp.options -e tcp.seq -e tcp.srcport -e tcp.urgent_pointer -e tcp.window_size -e udp.checksum -e udp.dstport -e udp.length -e udp.srcport
