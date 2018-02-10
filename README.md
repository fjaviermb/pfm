# pfm

## Tools/capture

A set of scripts to prepare raw data from original dataset. Current tools/scripts:

- **install.sh**: create links in Linux binary directory: "/usr/bin"
- **pcap_generate_week**: extract a week and adds in raw directory. Usage:
   ```
   pcap_generate_week.sh -w 1 -m training -y 1999 -t inside -d monday
   ```
- **pcap_tcpdump/pcap_tcpdump_extended**: show contents from capture in tcpdump binary format. Extended shows more fields.
- **pacap_tcpick**: show information from a capture. Only valid for TCP packets
- **tshark_export**:  export selected fields from a capture in tcpdump binay format. Exported fields are: ip.proto,eth.dst eth.type,eth.len,th.src,icmp.checksum, icmp.code, icmp.type, ip.checksum, ip.dst, ip.fragment, ip.frag_offset, ip.hdr_len, ip.len, ip.src, ip.tos, ip.ttl, tcp.ack, tcp.checksum, tcp.dstport, tcp.flags, tcp.hdr_len, tcp.options, tcp.seq, tcp.srcport, tcp.urgent_pointer, tcp.window_size, udp.checksum, udp.dstport, udp.length, udp.srcport

**Requirements**
"tshark" tool from WireShark mut be avilable. In case of CentOs 7, execute:
```shell
sudo yum install wireshark
sudo usermod -a -G wireshark nuctools
sudo reboot
```

"rstudio"
```shell
wget https://download1.rstudio.org/rstudio-1.1.423-x86_64.rpm
#https://www.rstudio.com/products/rpackages/devtools/
sudo yum install R R-dev
sudo rpm -i rstudio-1.1.423-x86_64.rpm
```

**Configuration**
Edit configuration file for base directories
```shell
nano  [INSTALLATION DIRECTORY]\config\global.conf
```
Change the following values if necessary:
```shell
BASE_DIR=$(realpath "$CONFIG_DIR/../.")
DATASETS_DIR=$BASE_DIR"/datasets/mitlinlab"
TOOLS_DIR=$BASE_DIR"/"tools
WORKSPACE_DIR=$BASE_DIR"/"workspace
RAW_DIR=$WORKSPACE_DIR"/"raw
```

**Installation**
```shell
cd [INSTALLATION DIRECTORY]\config
sudo sh install.sh
cd [INSTALLATION DIRECTORY]\tools\capture
sudo sh install.sh
```





