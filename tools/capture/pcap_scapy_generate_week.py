from scapy.all import *
import getopt, sys

# Capture python script. Read gzipped tcpdump files and exports to CSV file using pipe as field/column/value separator
# This code is based on and use code from ml-ids project by Luke (https://github.com/lukehsiao/ml-ids)


# This code is from ml-ids project: https://github.com/lukehsiao/ml-ids
# BEGINNING
TYPE_IPV4 = 0x0800
PROTO_ICMP = 1
PROTO_TCP = 6
PROTO_UDP = 17

FEATURES = [
    'Timestamp',
    'Ethernet_size',
    'Ethernet_dstHi',
    'Ethernet_dstLow',
    'Ethernet_srcHi',
    'Ethernet_srcLow',
    'Ethernet_type',
    'IPv4_ihl',
    'IPv4_tos',
    'IPv4_length',
    'IPv4_id',
    'IPv4_offset',
    'IPv4_ttl',
    'IPv4_proto',
    'IPv4_chksum',
    'IPv4_src',
    'IPv4_dst',
    'ICMP_type',
    'ICMP_code',
    'ICMP_chksum',
    'TCP_sport',
    'TCP_dport',
    'TCP_seqNo',
    'TCP_ackNo',
    'TCP_dataOffset',
    'TCP_flags',
    'TCP_window',
    'TCP_chksum',
    'TCP_urgPtr',
    'TCP_options',
    'UDP_sport',
    'UDP_dport',
    'UDP_length',
    'UDP_chksum',
]


def eprint(*args, **kwargs):
    """Print to stderr."""
    #print(*args, file=sys.stderr, **kwargs)
    print("TODO: Changed from original source")

def parse_pkt(pkt_bytes):
    """Parse the given packet byte string into a dictionary."""
    pkt = {}
    pkt['Ethernet'], pkt_bytes = parse_ethernet(pkt_bytes)
    if pkt['Ethernet']['type'] == TYPE_IPV4:
        pkt['IPv4'], pkt_bytes = parse_ipv4(pkt_bytes)
        if pkt['IPv4']['proto'] == PROTO_ICMP:
            pkt['ICMP'], pkt_bytes = parse_icmp(pkt_bytes)
        elif pkt['IPv4']['proto'] == PROTO_TCP:
            pkt['TCP'], pkt_bytes = parse_tcp(pkt_bytes)
        elif pkt['IPv4']['proto'] == PROTO_UDP:
            pkt['UDP'], pkt_bytes = parse_udp(pkt_bytes)
    return pkt


def parse_ethernet(pkt_bytes):
    """Parse the Ethernet header out of the given bytes."""
    total_len = 14
    eth = {}
    if len(pkt_bytes) < total_len:
        eprint("ERROR: not enough bytes to parse Ethernet header")
        return eth, pkt_bytes
    eth['dstHi'] = struct.unpack(">L", '\x00' + pkt_bytes[0:3])[0]
    eth['dstLow'] = struct.unpack(">L", '\x00' + pkt_bytes[3:6])[0]
    eth['srcHi'] = struct.unpack(">L", '\x00' + pkt_bytes[6:9])[0]
    eth['srcLow'] = struct.unpack(">L", '\x00' + pkt_bytes[9:12])[0]
    eth['type'] = struct.unpack(">H", pkt_bytes[12:14])[0]
    return eth, pkt_bytes[total_len:]


def parse_ipv4(pkt_bytes):
    total_len = 20
    ipv4 = {}
    if len(pkt_bytes) < total_len:
        eprint("ERROR: not enough bytes to parse IPv4 header")
        return ipv4, pkt_bytes
    # only want least significant bits
    ipv4['ihl'] = struct.unpack(">B", pkt_bytes[0:1])[0] & 0b00001111
    if (ipv4['ihl'] > 5):
        print("WARNING: did not parse ipv4 options from packet")
    ipv4['tos'] = struct.unpack(">B", pkt_bytes[1:2])[0]
    ipv4['length'] = struct.unpack(">H", pkt_bytes[2:4])[0]
    ipv4['id'] = struct.unpack(">H", pkt_bytes[4:6])[0]
    ## only want 3 most significant bits
    #ipv4['flags'] = struct.unpack(">B", pkt_bytes[6:7])[0] >> 5
    # get rid of most significant 3 bits
    ipv4['offset'] = struct.unpack(">H", pkt_bytes[6:8])[0] & 0b0001111111111111
    ipv4['ttl'] = struct.unpack(">B", pkt_bytes[8:9])[0]
    ipv4['proto'] = struct.unpack(">B", pkt_bytes[9:10])[0]
    ipv4['chksum'] = struct.unpack(">H", pkt_bytes[10:12])[0]
    ipv4['src'] = struct.unpack(">L", pkt_bytes[12:16])[0]
    ipv4['dst'] = struct.unpack(">L", pkt_bytes[16:20])[0]
    return ipv4, pkt_bytes[total_len:]


def parse_icmp(pkt_bytes):
    total_len = 4
    icmp = {}
    if len(pkt_bytes) < total_len:
        eprint("ERROR: not enough bytes to parse ICMP header")
        return icmp, pkt_bytes
    icmp['type'] = struct.unpack(">B", pkt_bytes[0:1])[0]
    icmp['code'] = struct.unpack(">B", pkt_bytes[1:2])[0]
    icmp['chksum'] = struct.unpack(">H", pkt_bytes[2:4])[0]
    return icmp, pkt_bytes[total_len:]


def parse_tcp(pkt_bytes):
    total_len = 20
    tcp = {}
    if len(pkt_bytes) < total_len:
        eprint("ERROR: not enough bytes to parse TCP header")
        return tcp, pkt_bytes
    tcp['sport'] = struct.unpack(">H", pkt_bytes[0:2])[0]
    tcp['dport'] = struct.unpack(">H", pkt_bytes[2:4])[0]
    tcp['seqNo'] = struct.unpack(">L", pkt_bytes[4:8])[0]
    tcp['ackNo'] = struct.unpack(">L", pkt_bytes[8:12])[0]
    # just the 4 most significant bits
    tcp['dataOffset'] = struct.unpack(">B", pkt_bytes[12:13])[0] >> 4
    # & 0b11000000 # dont care about 2 most signifiant bits
    tcp['flags'] = struct.unpack(">B", pkt_bytes[13:14])[0]
    tcp['window'] = struct.unpack(">H", pkt_bytes[14:16])[0]
    tcp['chksum'] = struct.unpack(">H", pkt_bytes[16:18])[0]
    tcp['urgPtr'] = struct.unpack(">H", pkt_bytes[18:20])[0]
    if (tcp['dataOffset'] > 5):
        tcp['options'] = struct.unpack(">L", pkt_bytes[20:24])[0]
        total_len += 4
    return tcp, pkt_bytes[total_len:]


def parse_udp(pkt_bytes):
    total_len = 8
    udp = {}
    if len(pkt_bytes) < total_len:
        eprint("ERROR: not enough bytes to parse UDP header")
        return udp, pkt_bytes
    udp['sport'] = struct.unpack(">H", pkt_bytes[0:2])[0]
    udp['dport'] = struct.unpack(">H", pkt_bytes[2:4])[0]
    udp['length'] = struct.unpack(">H", pkt_bytes[4:6])[0]
    udp['chksum'] = struct.unpack(">H", pkt_bytes[6:8])[0]
    return udp, pkt_bytes[total_len:]


# This code is from ml-ids project: https://github.com/lukehsiao/ml-ids/blob/master/utils/pcap_parser.py
# END

# Adjust the time in order to be in the same time zone as the docmentation in the dataset
ADJUSTMENT_TIME = -5 * 3600

def mac2str(mac_hi,mac_low):
    mac_arr = [mac_hi,mac_low]
    mac_str =''.join('{:06X}'.format(mac_dec) for mac_dec in mac_arr)
    return ':'.join( mac_str[i]+mac_str[i+1] for i in range(0,len(mac_str),2))

def ip2str(ip):
    return socket.inet_ntoa(struct.pack('!L',ip))

def chksum2str(chksum):
    return '0x'+'{:04X}'.format(chksum)


def main(argv):

    raw_dir = ""
    dataset_dir = ""
    year = "1999"
    mode = "training"
    week = "week1"
    day = "friday"
    type = "inside"
    separator="|"

    try:
        opts, args = getopt.getopt(argv,'w:m:d:t:y:r:a:s:')
    except getopt.GetoptError:
        print("error")
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-s", "--separator"):
            separator=arg
        elif opt in ("-w","--week"):
            week=arg
        elif opt in ("-m","--mode"):
            mode=arg
        elif opt in ("-d","--day"):
            day=arg
        elif opt in ("-t","--type"):
            type=arg
        elif opt in ("-y","--year"):
            year=arg
        elif opt in ("-r","--rawdir"):
            raw_dir=arg
        elif opt in ("-a","--datasetdir"):
            dataset_dir=arg

    filename=year+"_"+mode+"_week"+week+"_"+day+"_"+type+".tcpdump.gz"
    filedir=dataset_dir+"/week"+week
    input_file=filedir+"/"+filename
    output_filename=raw_dir+"/"+year+"_"+mode+"_week"+week+"_"+day+"_"+type+".csv"

    totalbytes = 0

    # epoch: number of seconds from 1970
    # usec: microseconds
    # pktlen: paket len

    register = {}
    for pkt_bytes1, (epoch, usec, pktlen) in RawPcapReader(input_file):

        pkt = parse_pkt(pkt_bytes1)

        # Initialization
	register['Timestamp']=None
        register['Ethernet_size']=None
        register['Ethernet_dstHi']=None
        register['Ethernet_dstLow']=None
        register['Ethernet_srcHi']=None
        register['Ethernet_srcLow']=None
        register['Ethernet_type']=None
        register['IPv4_ihl']=None
        register['IPv4_tos']=None
        register['IPv4_length']=None
        register['IPv4_id']=None
        register['IPv4_offset']=None
        register['IPv4_ttl']=None
        register['IPv4_proto']=None
        register['IPv4_chksum']=None
        register['IPv4_src']=None
        register['IPv4_dst']=None
        register['ICMP_type']=None
        register['ICMP_code']=None
        register['ICMP_chksum']=None
        register['TCP_sport']=None
        register['TCP_dport']=None
        register['TCP_seqNo']=None
        register['TCP_ackNo']=None
        register['TCP_dataOffset']=None
        register['TCP_flags']=None
        register['TCP_window']=None
        register['TCP_chksum']=None
        register['TCP_urgPtr']=None
        register['TCP_options']=None
        register['UDP_sport']=None
        register['UDP_dport']=None
        register['UDP_length']=None
        register['UDP_chksum']=None

	# Add timestamp
	#register['Timestamp'] = (float(epoch) * float(1000000)) + float(usec)
	register['Timestamp'] = str(epoch + ADJUSTMENT_TIME) + str(usec).zfill(6)


        # Fill Ethernet fields
        #register['Ethernet_size'] = pkt['Ethernet']['size']
        register['Ethernet_size'] = pktlen
        register['Ethernet_dstHi'] = pkt['Ethernet']['dstHi']
        register['Ethernet_dstLow'] = pkt['Ethernet']['dstLow']
        register['Ethernet_srcHi'] = pkt['Ethernet']['srcHi']
        register['Ethernet_srcLow'] = pkt['Ethernet']['srcLow']
        register['Ethernet_type'] = pkt['Ethernet']['type']

        if pkt['Ethernet']['type'] == TYPE_IPV4:
            register['IPv4_ihl']=pkt['IPv4']['ihl']
            register['IPv4_tos']=pkt['IPv4']['tos']
            register['IPv4_length']=pkt['IPv4']['length']
            register['IPv4_id']=pkt['IPv4']['id']
            register['IPv4_offset']=pkt['IPv4']['offset']
            register['IPv4_ttl']=pkt['IPv4']['ttl']
            register['IPv4_proto']=pkt['IPv4']['proto']
            register['IPv4_chksum']=pkt['IPv4']['chksum']
            register['IPv4_src']=pkt['IPv4']['src']
            register['IPv4_dst']=pkt['IPv4']['dst']

            if pkt['IPv4']['proto'] == PROTO_ICMP:
                register['ICMP_type']=pkt['ICMP']['type']
                register['ICMP_code']=pkt['ICMP']['code']
                register['ICMP_chksum']=pkt['ICMP']['chksum']

            if pkt['IPv4']['proto'] == PROTO_TCP:
                register['TCP_sport']=pkt['TCP']['sport']
                register['TCP_dport']=pkt['TCP']['dport']
                register['TCP_seqNo']=pkt['TCP']['seqNo']
                register['TCP_ackNo']=pkt['TCP']['ackNo']
                register['TCP_dataOffset']=pkt['TCP']['dataOffset']
                register['TCP_flags']=pkt['TCP']['flags']
                register['TCP_window']=pkt['TCP']['window']
                register['TCP_chksum']=pkt['TCP']['chksum']
                register['TCP_urgPtr']=pkt['TCP']['urgPtr']
                if 'options' in pkt['TCP'].keys():
                    register['TCP_options']=pkt['TCP']['options']

            if pkt['IPv4']['proto'] == PROTO_UDP:
                register['UDP_sport']=pkt['UDP']['sport']
                register['UDP_dport']=pkt['UDP']['dport']
                register['UDP_length']=pkt['UDP']['length']
                register['UDP_chksum']=pkt['UDP']['chksum']

        # Convert to string or put empty string for None
        for key in register:
            if register[key] == None:
                register[key] = "";
            else:
                register[key] = str(register[key])

        if pkt['Ethernet']['type'] == TYPE_IPV4 and ( pkt['IPv4']['proto'] == PROTO_TCP or pkt['IPv4']['proto'] == PROTO_UDP or pkt['IPv4']['proto'] == PROTO_ICMP):
               print( register['Timestamp'] + separator + register['Ethernet_size'] + separator+ register['Ethernet_dstHi'] + separator+ register['Ethernet_dstLow'] + separator+ register['Ethernet_srcHi'] + separator+ register['Ethernet_srcLow'] + separator+ register['Ethernet_type'] + separator+ register['IPv4_ihl'] + separator+ register['IPv4_tos'] + separator+ register['IPv4_length'] + separator+ register['IPv4_id'] + separator+ register['IPv4_offset'] + separator+ register['IPv4_ttl'] + separator+ register['IPv4_proto'] + separator+ register['IPv4_chksum'] + separator+ register['IPv4_src'] + separator+ register['IPv4_dst'] + separator+ register['ICMP_type'] + separator+ register['ICMP_code'] + separator+ register['ICMP_chksum'] + separator+ register['TCP_sport'] + separator+ register['TCP_dport'] + separator+ register['TCP_seqNo'] + separator+ register['TCP_ackNo'] + separator+ register['TCP_dataOffset'] + separator+ register['TCP_flags'] + separator+ register['TCP_window'] + separator+ register['TCP_chksum'] + separator+ register['TCP_urgPtr'] + separator+ register['TCP_options'] + separator+ register['UDP_sport'] + separator+ register['UDP_dport'] + separator+ register['UDP_length'] + separator+ register['UDP_chksum'] )


if __name__ == "__main__":
    main(sys.argv[1:])


