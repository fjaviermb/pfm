library("tidyr")

#' Load dataset from all files included in the input.dir path
#' 
#' @description Read all files contained into <input.dir>. Each file is a CSV file. The file format is:
#'
#'   Column01: dt.year. Dataset year.
#'   Column02: dt.mode. Dataset purpose mode:  training, evaluation.
#'   Column03: dt.week. Dataset week.
#'   Column04: dt.day. Dataset day of the week.
#'   Column05: dt.type. Dataset capture type (inside, outside, etc.).
#'   Column06: ip.proto. IP Protocol.
#'   Column07: eth.dst. Destination MAC address.
#'   Column08: eth.type. Ethernet type.
#'   Column09: eth.len. Ethernet frame lenght.
#'   Column10: eth.src. Source MAC address.
#'   Column11: icmp.checksum. CRC ICMP.
#'   Column12: icmp.code. ICMP code.
#'   Column13: icmp.type. ICMP type.
#'   Column14: ip.checksum. IP CRC.
#'   Column15: ip.dst. Destination IP.
#'   Column16: ip.fragment. Fragment number.
#'   Column17: ip.frag_offset. Fragment offset.
#'   Column18: ip.hdr_len. IP Header length.
#'   Column19: ip.len. IP Pacakage Lenght.
#'   Column20: ip.src. Source IP.
#'   Column21: ip.tos. Type of service.
#'   Column22: ip.ttl. IP Time-To-Life.
#'   Column23: tcp.ack. Flag ACK.
#'   Column24: tcp.checksum. TCP CRC.
#'   Column25: tcp.dstport. Destination Port.
#'   Column26: tcp.flags. Flags.
#'   Column27: tcp.hdr_len. TCP Header Length.
#'   Column28: tcp.options. TCP Options.
#'   Column29: tcp.seq. TCP sequence.
#'   Column30: tcp.srcport. Source Port.
#'   Column31: tcp.urgent_pointer. Urgent Pointer.
#'   Column32: tcp.window_size. Window Size.
#'   Column33: udp.checksum. UDP CRC.
#'   Column34: udp.dstport. UDP destination Port.
#'   Column35: udp.length. UDP length.
#'   Column36: udp.srcport. UDP source Port.
#
#' @example: loadDataset input/file...
#' @param input.dir Path containing dataset input files
loadDataset <- function(input.dir) {
  
  
  
  for (file in list.files(input.dir)) {
    
    dataset.entries <- read.csv(paste(input.dir,file,sep="/"),header=FALSE,sep=",",stringsAsFactors = FALSE)
    #computer.entries$computer <- rep(file,nrow(computer.entries))
    
    if (exists('all.dataset.entries')) {
      all.dataset.entries <- dplyr::union(all.dataset.entries,dataset.entries)
    } else {
      all.dataset.entries <- dataset.entries
    }
    break
  }
  
  names(all.dataset.entries) <- c("dt.year","dt.mode","dt.week","dt.day","dt.type","ip.proto","eth.dst","eth.type","eth.len","eth.src","icmp.checksum","icmp.code","icmp.type","ip.checksum","ip.dst","ip.fragment","ip.frag_offset","ip.hdr_len","ip.len","ip.src","ip.tos","ip.ttl","tcp.ack","tcp.checksum","tcp.dstport","tcp.flags","tcp.hdr_len","tcp.options","tcp.seq","tcp.srcport","tcp.urgent_pointer","tcp.window_size","udp.checksum","udp.dstport","udp.length","udp.srcport") 
  
  return(all.dataset.entries)

}

#dataset.entries <- read.csv(paste(input.dir,file,sep="/"),header=FALSE,sep=",",stringsAsFactors = FALSE)

# Download CPE/CVE data from github
#
# @param input.dir destination directory to put the downloaded file relative to current workspace home location
# @example downloadSysdata input
downloadSysdata <- function(input.dir) {
  
  url <- "https://github.com/r-net-tools/security.datasets/raw/master/net.security/sysdata.rda"
  path <- paste(input.dir,"sysdata.rda",sep = "/")
  download.file(url = url, path)
  
}


#' Load files containing the relation beetween the computer and the company criticity 
#'
#' @param input.file CSV file containing the information, The format of the file is
#'         First line: header containing the titles.
#'         Column1: computer name
#'         Column2: Criticity. Number: 0 no critial, 1 critical
#' @param random  to generate an artificial input
#' @param computer.entries in case of random=TRUE, the list of computers. It takes computers names from this dataset.
loadComputerCriticity <- function(input.file, computer.entries=NULL, random=FALSE) {
  
  computers.entries.criticity <- as.data.frame(x=c())
  
  if( random && !is.null(computers.entries)) {
    # Generación aleatoria de criticidad
    computers.entries.criticity <- dplyr::select(computers.entries,computer) %>% distinct() 
    computers.entries.criticity$criticidad <- as.data.frame(sample(x=rep(x=1:nrow(computers.entries.criticity)%%2)))
  } else {
    computers.entries.criticity <- read.csv(input.file,header=TRUE,sep=";",stringsAsFactors = FALSE)
  }
 
  names(computers.entries.criticity) <- c("computer","criticidad")
  
  return(computers.entries.criticity)
  
}

# Load simulation data for testing visualization phase
#
# @param root.dir project root dir to find CSV
#
loadScoringTesting <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,"samples/input/visualization",'cache',sep='/')
  file.name <- 'scoring_test.csv'
  file.fullname <- paste( input.dir,file.name,sep="/")
  computers.entries.scoring <- read.table(file.name,header=T,sep=";")
  return(computers.entries.scoring)

}