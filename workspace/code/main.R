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
# install.packages("tictoc")
# install.packages("hash")
library("dplyr")
library("tidyr")
library("ggplot2")


# Clear environment
rm(list=ls())

# Should be the "code" folder
root.dir <- paste(getwd(),"code", sep="/")

#https://stackoverflow.com/questions/12642651/in-r-how-to-get-error-messages-in-english
## English messages
Sys.setlocale("LC_MESSAGES", "C")

#https://stackoverflow.com/questions/9397664/force-r-not-to-use-exponential-notation-e-g-e10
## Avoid scientific representation. This option aids to print R command to display a number in scientific/exponential representation or not
options("scipen"=100, "digits"=4)

# Load scripts
source(paste(root.dir,"input/inputLoader.R",sep="/"))
source(paste(root.dir,"util/clusterer.R",sep="/"))
source(paste(root.dir,"phad-c32.R",sep="/"))



getWorker <- function() {
  
  # Get current worker. By default no worker or invalid worker
  worker.current <- -1
  
  if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
    worker.all <- as.numeric(Sys.getenv("worker_all"))
    worker.current <- as.numeric(Sys.getenv("worker_current"))  
  }
  
  return (worker.current)
  
}


calculateModel <- function(root.dir = getwd(), cache = TRUE, wof ) {
  
  # if cache is enabled, try to load fisrt the model, if not, continue normal flow
  if( cache ) {
    model.ds = loadCacheModel(root.dir)
  }
  
  if( is.null (model.ds) ) {
    
    # Load tranining data
    training.raw.ds <- loadTrainingDataset(cache, root.dir)
    
    # Calculate the model
    model.ds <- train(training.raw.ds, cache, root.dir,wof)
    
    # Release resources  
    rm(training.raw.ds)
    gc()
    
  }
  
  return(model.ds)  
  
}

worker.current <- getWorker()

logger("Starting process..")
model.ds <- calculateModel(root.dir, cache = TRUE, wof = NULL)
logger("Model calculated")
# Load master attack list
label.attacklist.raw.ds <- loadLabelAttackList(cache = FALSE, root.dir)
logger("Label attack list loaded")
# Load tranining data
testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )
logger("Testing data loaded")
# Load label data
label.testing.raw.ds <- labelTestingDS(testing.raw.ds, label.attacklist.raw.ds, TRUE, root.dir)
logger("Label data loaded")

# Load scoring features
# Select which features are going to claculate by this worker
# i.e.: wof[31] <- worker.current
wof <- rep(-1,33)
#wof[1]<-worker.current
wof[20]<-worker.current

#testing.raw.ds %>% filter( timestamp >= 922677515) %>% filter( timestamp <= 922677762)
scoresAll <- scoring(model.ds,testing.raw.ds, label.testing.raw.ds, FALSE, root.dir, wof)
#scoresAll <- scoringAggregate(model.ds,testing.raw.ds, label.testing.raw.ds, TRUE, root.dir)
