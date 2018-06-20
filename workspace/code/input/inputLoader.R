library("tidyr")
require(tictoc)
source(paste(root.dir,"cache/cacheLoader.R",sep="/"))

PATTERN_TRAINING <- ".*training.*"
PATTERN_TESTING <- ".*testing.*"
#PATTERN_TESTING <- "1999_testing_week4_monday_inside.csv"
PATTERN_ATTACK_LIST <- "master-listfile-condensed.csv"
INPUT_RAW_DIR <- "raw"


getRawDir <- function(root.dir = getwd()) {
  
  return( paste(root.dir,"..","raw",sep="/"))
  
}

#' Load training dataset
loadTrainingDataset <- function(cache = FALSE, root.dir=getwd()) {
  
  if( cache ) {
    
    training.raw.ds <- loadCacheTrainingRaw(root.dir) 
    
  } else {
    
    filelist = c(
      '1999_training_week3_monday_inside.csv',
      '1999_training_week3_extra_monday_inside.csv',
      '1999_training_week3_tuesday_inside.csv',
      '1999_training_week3_extra_tuesday_inside.csv',
      '1999_training_week3_wednesday_inside.csv',
      '1999_training_week3_extra_wednesday_inside.csv',
      '1999_training_week3_thursday_inside.csv',
      '1999_training_week3_friday_inside.csv'
      )
      
    training.raw.ds <- loadDataset(getRawDir(root.dir), pattern =  NULL, filelist = filelist)
    
  }
  
  return(training.raw.ds)
  
}

#' Load testing dataset
#' Load testing dataset
loadTestingDataset <- function(cache = FALSE, root.dir = getwd() ) {
  
  if( cache ) {
    
    testing.raw.ds <- loadCacheTestingRaw(root.dir) 
    
  } else {
    
    filelist = c(
      '1999_testing_week4_monday_inside.csv',
      '1999_testing_week4_wednesday_inside.csv',
      '1999_testing_week4_thursday_inside.csv',
      '1999_testing_week4_friday_inside.csv',
      '1999_testing_week5_monday_inside.csv',
      '1999_testing_week5_tuesday_inside.csv',
      '1999_testing_week5_wednesday_inside.csv',
      '1999_testing_week5_thursday_inside.csv',
      '1999_testing_week5_friday_inside.csv'
      
    )
    
    testing.raw.ds.tmp  <- loadDataset(getRawDir(root.dir), pattern =  NULL, filelist = filelist)
    
    
    #testing.raw.ds.tmp <- loadDataset(getRawDir(root.dir), PATTERN_TESTING)
    testing.raw.ds <- testing.raw.ds.tmp %>% arrange(timestamp)
    
    
  }
  
  return(testing.raw.ds)
  
}

#' Load attack list to mark an entry as an attack or not during the testing phase
#' 
#' @description Read file containing entries considered as attacks. Unique file as a CSV file. The file format is:
#'
#'    Column01: id		      Entry id as float.
#'    Column02: dstIP		    Destination IP as long format.
#'    Column03: starttime		Timestamp when the attack starts.
#'    Column04: endtime		  Timestamp when the attack ends.
#'    Column05: destIPstr		Destination IP as string/literal using 4 decimals separate using "." (just the same as dstIP but different format)
#'    Column06: name	      Attack name
#'    
#' @example: loadDataset input/file...
#' @param cache     Do not do any calculation, use precalculated data
#' @param root.dir  Workspace root directory used to load cached data
loadLabelAttackList <- function(cache = FALSE, root.dir=getwd()) {
  
  if( cache ) {
    
    label.attacklist.raw.ds <- loadCacheLabelAttackListRaw(root.dir) 
    
  } else {
    
    #  Path containing dataset input files
    input.dir <- getRawDir(root.dir)
    
    for (file in list.files(input.dir, pattern = PATTERN_ATTACK_LIST)) {
      
      label.attacklist.raw.ds <- read.csv(paste(input.dir,file,sep="/"),header=FALSE,sep="|",stringsAsFactors = FALSE,
                                          colClasses=c("character","double","double","double","character","character")
      )
      
    }
    
    if( ! exists("label.attacklist.raw.ds") ) {
      print(paste("WARNING: No label attack files found, creating an empty vector (input directory[",input.dir,"]"))
      label.attacklist.raw.ds <- c("0","0",0,0,0,"0")
    }
    
    names(label.attacklist.raw.ds) <- c("id","dstIP","starttime","endtime","destIPstr","name");
    
  }  
  
  return(label.attacklist.raw.ds)
  
}


#' Load dataset from all files included in the input.dir path
#' 
#' @description Read all files contained into <input.dir>. Each file is a CSV file. The file format is:
#'
#'    Column01: dt.year		  Dataset year.
#'    Column02: dt.mode		  Dataset purpose mode- training, evaluation.
#'    Column03: dt.week		  Dataset week.
#'    Column04: dt.day		  Dataset day of the week.
#'    Column05: dt.type		  Dataset capture type (inside, outside, etc.).
#'    Column06: timestamp	  Capture timestamp  microsec
#'    Column07: eth.size	  Ethernet frame lenght.
#'    Column08: eth.dstHi	  Destination MAC address Hi.
#'    Column09: eth.dstLow	Destination MAC address Low.
#'    Column10: eth.srcHi	  Source MAC address Low.
#'    Column11: eth.srcLow	Source MAC address Hi.
#'    Column12: eth.type	  Ethernet type.
#'    Column13: ip.ihl		  IP Header length.
#'    Column14: ip.tos		  Type of service.
#'    Column15: ip.length	  IP Pacakage Lenght.
#'    Column16: ip.id		    Fragment number.
#'    Column17: ip.offset	  Fragment offset.
#'    Column18: ip.ttl		  IP Time-To-Life.
#'    Column19: ip.proto	  IP Protocol.
#'    Column20: ip.chksum	  IP CRC.
#'    Column21: ip.src		  Source IP.
#'    Column22: ip.dst		  Destination IP.
#'    Column23: icmp.type	  ICMP type.
#'    Column24: icmp.code	  ICMP code.
#'    Column25: icmp.chksum	CRC ICMP.
#'    Column26: tcp.sport	  Source Port.
#'    Column27: tcp.dport	  Destination Port.
#'    Column28: tcp.seqNo	  TCP sequence.
#'    Column29: tcp.ackNo	  Flag ACK.
#'    Column30: tcp.dataOffset	TCP Header Length / data offset
#'    Column31: tcp.flags	  Flags.
#'    Column32: tcp.window	Window Size.
#'    Column33: tcp.chksum	TCP CRC.
#'    Column34: tcp.urgPtr	Urgent Pointer.
#'    Column35: tcp.options	TCP Options.
#'    Column36: udp.sport	  UDP source Port.
#'    Column37: udp.dport	  UDP destination Port.
#'    Column38: udp.length	UDP length.
#'    Column39: udp.chksum	UDP CRC.
#
#' @example: loadDataset input/file...
#' @param input.dir Path containing dataset input files
#' @param root.dir  Workspace root directory used to load cached data
#' @param filelist  Specific file list. If it is NULL, pattern option is used 
#' @param pattern   Filer fiels in raw directory. IfDefault: all files pattern (.*)
loadDataset <- function(input.dir, pattern = NULL, filelist = NULL) {
  
  if( is.null(filelist ) ) {
    filelist = list.files(input.dir, pattern = pattern)
  }

  for (file in filelist ) {  
  
      tic()
    
      print(paste("Cargando archivo [",file,"]"))
  
      dataset.entries <- read.csv(paste(input.dir,file,sep="/"),header=FALSE,sep="|",stringsAsFactors = FALSE,
                                colClasses=c("character","character","character","character","character","double","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
    )
     
    if (exists('all.dataset.entries')) {
      all.dataset.entries <- dplyr::bind_rows(all.dataset.entries,dataset.entries)
    } else {
      all.dataset.entries <- dataset.entries
    }
    
    rnorm(1000,0,1)
    toc()
  
  }
  
  if( !exists( "all.dataset.entries" ) ) {
    print(paste("Error, no files found in [",input.dir,"] with PATTERN [",pattern,"]"))
  }
  
  names(all.dataset.entries) <- c("dt.year","dt.mode","dt.week","dt.day","dt.type","timestamp","eth.size","eth.dstHi","eth.dstLow","eth.srcHi","eth.srcLow","eth.type","ip.ihl","ip.tos","ip.length","ip.id","ip.offset","ip.ttl","ip.proto","ip.chksum","ip.src","ip.dst","icmp.type","icmp.code","icmp.chksum","tcp.sport","tcp.dport","tcp.seqNo","tcp.ackNo","tcp.dataOffset","tcp.flags","tcp.window","tcp.chksum","tcp.urgPtr","tcp.options","udp.sport","udp.dport","udp.length","udp.chksum") 
  
  return(all.dataset.entries)
  
}


#' Load dataset from all files included in the input.dir path
#' 
#' @description Read all files contained into <input.dir>. Each file is a CSV file. The file format is:
#'
#'   Column01: dt.year		    Dataset year.
#'   Column02: dt.mode		    Dataset purpose mode:  training, evaluation.
#'   Column03: dt.week		    Dataset week.
#'   Column04: dt.day		      Dataset day of the week.
#'   Column05: dt.type		    Dataset capture type (inside, outside, etc.).
#'   Column06: timestamp	    Timestamp
#'   Column07: ip_proto	 	    Ethernet frame lenght.
#'   Column08: eth_dst	 	    Destination MAC address Hi.
#'   Column09: eth_type	 	    Destination MAC address Low.
#'   Column10: eth_len	 	    Source MAC address Low.
#'   Column11: eth_src	 	    Source MAC address Hi.
#'   Column12: icmp_checksum	Ethernet type.
#'   Column13: icmp_code	    IP Header length.
#'   Column14: icmp_type  	  Type of service.
#'   Column15: ip_checksum	  IP Pacakage Lenght.
#'   Column16: ip_dst	 	      Fragment number.
#'   Column17: ip_fragment	  Fragment offset.
#'   Column18: ip_frag_offset	IP Time-To-Life.
#'   Column19: ip_hdr_len	    IP Protocol.
#'   Column20: ip_len	 	IP CRC.
#'   Column21: ip_src	 	Source IP.
#'   Column22: ip_tos	 	Destination IP.
#'   Column23: ip_ttl	 	ICMP type.
#'   Column24: tcp_ack	 	ICMP code.
#'   Column25: tcp_checksum	CRC ICMP.
#'   Column26: tcp_dstport	Source Port.
#'   Column27: tcp_flags	Destination Port.
#'   Column28: tcp_hdr_len	TCP sequence.
#'   Column29: tcp_options	Flag ACK.
#'   Column30: tcp_seq	 	TCP Header Length / data offset
#'   Column31: tcp_srcport	Flags.
#'   Column32: tcp_urgent_pointer	Window Size.
#'   Column33: tcp_window_size	TCP CRC.
#'   Column34: udp_checksum	Urgent Pointer.
#'   Column35: udp_dstport	TCP Options.
#'   Column36: udp_length	  UDP source Port.
#'   Column37: udp_srcport	UDP destination Port.
#'   Column38: udp.length	  UDP length.
#'   Column39: udp.chksum	  UDP CRC.
#
#' @example: loadDataset input/file...
#' @param input.dir Path containing dataset input files
#' @deprecated This is the first version based on test using tshark to extract informacion from tcdump files. Use "loadDataset" function instead
loadDatasetV0 <- function(input.dir) {
  
  for (file in list.files(input.dir)) {
    
    dataset.entries <- read.csv(paste(input.dir,file,sep="/"),header=FALSE,sep=",",stringsAsFactors = FALSE)
    
    if (exists('all.dataset.entries')) {
      # all.dataset.entries <- dplyr::union(all.dataset.entries,dataset.entries)
      all.dataset.entries <- dplyr::bind_rows(all.dataset.entries,dataset.entries)
     } else {
      all.dataset.entries <- dataset.entries
    }
    break
  }
  
  names(all.dataset.entries) <- c("dt.year","dt.mode","dt.week","dt.day","dt.type","ip.proto","eth.dst","eth.type","eth.len","eth.src","icmp.checksum","icmp.code","icmp.type","ip.checksum","ip.dst","ip.fragment","ip.frag_offset","ip.hdr_len","ip.len","ip.src","ip.tos","ip.ttl","tcp.ack","tcp.checksum","tcp.dstport","tcp.flags","tcp.hdr_len","tcp.options","tcp.seq","tcp.srcport","tcp.urgent_pointer","tcp.window_size","udp.checksum","udp.dstport","udp.length","udp.srcport") 
  
  return(all.dataset.entries)
  
}


## EXTRA: Load index
loadIndex <- function() {
  
  file <- '/home/nuctools/workspace/gitrepos/repos/ml-ids/index_ts_neto.txt'
  index.ds <- read.csv(file,header=FALSE,sep=",",stringsAsFactors = FALSE,colClasses=c("double"))
  names(index.ds) <- c('timestamp')
  return(index.ds)
}

sortTraining <- function(index.ds, training.raw.ds) {
  
  require(gdata)
  index <- as.vector(index.ds['timestamp'])
  
  #training.raw.ds[match(index, training.raw.ds$timestamp,]
  
  training.raw.ds$timestamp <- reorder.factor(training.raw.ds$timestamp, new.order=index)
  require(dplyr)
  training.raw.ds.sorted <- training.raw.ds %>% arrange(timestamp)
  
  
  
}

