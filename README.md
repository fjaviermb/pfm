# pfm
Implements an Anomaly detector based on network traffic. Based on PHAD-C32 (PHAD: Packet Header Anomaly Detection for Identifying Hostile Network Traffic).

Contains tools to parser data from input cap files and the implementation of the Anomaly Detector writeen in R (net.phadc32).

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

- **install.sh**: creates links in Linux binary directory: "/usr/bin"
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
In the current edvelopment, the entry point (main function) is on "net.phadc32/R/main.R"
```R
logger("Calculate model")
model.ds <- calculateModel(root.dir, cache = TRUE, wof = NULL)
logger("Evaluate model")
testing.raw.scored.ds <- evaluateModel(model.ds, root.dir, cache = TRUE, wof = NULL)
logger("Measure results")
testing.measured.ds <- measureResults(testing.raw.scored.ds, root.dir, cache)
rm(testing.raw.scored.ds)
gc()
```
