# pfm

## Dataset

   The dataset used is the 1999 DARPA Intrusion detection from Lincoln Laboratory (MIT - https://www.ll.mit.edu/ideval/data/1999data.html)

    ** Training: week three (inside traffic).
    ** Testing: weeks four and five (inside traffic).

   Using the scripts below, to extract "text" data from tcpdmup binary capture files, the following commands are launched:
      
```
# TRAINING
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d friday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d monday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d tuesday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d thursday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d wednesday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d friday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d extra_monday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d extra_wednesday
pcap_scapy_generate_week.sh -w 3 -m training -y 1999 -t inside -s \| -d extra_tuesday
```

```
# TESTING/EVALUATION
pcap_scapy_generate_week.sh -w 4 -m testing -y 1999 -t inside -s \| -d monday
pcap_scapy_generate_week.sh -w 4 -m testing -y 1999 -t inside -s \| -d wednesday
pcap_scapy_generate_week.sh -w 4 -m testing -y 1999 -t inside -s \| -d thursday
pcap_scapy_generate_week.sh -w 4 -m testing -y 1999 -t inside -s \| -d friday
pcap_scapy_generate_week.sh -w 5 -m testing -y 1999 -t inside -s \| -d monday
pcap_scapy_generate_week.sh -w 5 -m testing -y 1999 -t inside -s \| -d tuesday
pcap_scapy_generate_week.sh -w 5 -m testing -y 1999 -t inside -s \| -d wednesday
pcap_scapy_generate_week.sh -w 5 -m testing -y 1999 -t inside -s \| -d thursday
pcap_scapy_generate_week.sh -w 5 -m testing -y 1999 -t inside -s \| -d friday
```

## Capure/ingestion tools

A set of scripts to prepare raw data from original dataset. Current tools/scripts:

- **install.sh**: create links in Linux binary directory: "/usr/bin"
- **pcap_scapy_generate_week**: extract a week and adds in raw directory. Usage:
   ```
   pcap_scapy_generate_week.sh -w 1 -m training -y 1999 -t inside -d monday
   ```
   Options:
   - **w|week**: week to recover from dataset
   - **m|mode**: logical algorithm mode: training, testing
   - **d|day**:  day pof the week
   - **y|year**: year of the dataset
   - **t|type**: inside or outside (depends on dataset)
   
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


## Trainig and evaluation runtime

Current code is being developed using R and RStudio is used as R IDE (you can use whatever you want).

### Installing RStudio on CentOS 7
```shell
wget https://download1.rstudio.org/rstudio-1.1.423-x86_64.rpm
#https://www.rstudio.com/products/rpackages/devtools/
sudo yum install R R-dev
sudo rpm -i rstudio-1.1.423-x86_64.rpm
```

### Running code
In the current edvelopment, the entry point (main function) is on "workspace/code/main.R"