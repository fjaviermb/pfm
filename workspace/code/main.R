# Utility package method
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x)
    require(x,character.only=TRUE)
  }
}

# packages("ngram")
# packages("stringdist")
# packages("tidyr")
# packages("dplyr")
#   Required on CentOS to install devtools: yum install libcurl-devel
#   install.packages("devtools")
# packages("devtools")
#   devtools::install_github(repo = "r-net-tools/net.security")
#   sudo yum install libxml2-devel required by XML package on CentOS 7
#   sudo yum install openssl-devel required by devtools/gitr package on CentOS 7
#   install.packages("XML") required by net.security
# packages("XML")
# packages("net.security")
# packages("sets")


#library("devtools")
#library("XML")
#library("ngram")
#library("stringdist")

# Do not load, %>% in dpylr and tidylr is masked by sets
#library("sets")
# detach("package:sets", unload=TRUE)
# install.packages("dplyr")
# install.packages("tidyr")
library("dplyr")
library("tidyr")



# Clear environment
rm(list=ls())

rootDir <- "~/git-repos/pfm/workspace/code"


# Load scripts
source(paste(rootDir,"input/inputLoader.R",sep="/"))

INPUT_DS_RAW_DIR <-paste(rootDir,"../raw",sep="/")



# 1 Load official cve/cpes
#downloadSysdata(INPUT_SECURITY_DIR)
#load("~/workspace/gitrepos/repos/PracticaDDS/input/sysdata.rda")

# All CVEs/CPEs. Take care of that, a lot of data is loaded
#cves <- netsec.data$datasets$cves
#cpes <- netsec.data$datasets$cpes

# Only load a sample from all CVEs/CPEs.
#cpes <- net.security::GetDataFrame("cpes") 
#cves <- net.security::GetDataFrame("cves")

# 2 Collect data from PCs
dataset.entries <- loadDataset(INPUT_DS_RAW_DIR)