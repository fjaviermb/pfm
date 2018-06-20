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

# Load master attack list
label.attacklist.raw.ds <- loadLabelAttackList(cache = TRUE, root.dir)

# Load tranining data
training.raw.ds <- loadTrainingDataset(cache = TRUE, root.dir)


# Create an array to calculate specific features of the model
# Set don't do any feature calculation. It is possible to initialitze wof to null (wof <-NULL)
wof <- rep(-1,33)

# Get current worker
worker.current <- 0

if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
  worker.all <- as.numeric(Sys.getenv("worker_all"))
  worker.current <- as.numeric(Sys.getenv("worker_current"))  
}

# Select which features are going to claculate by this worker
# i.e.: wof[31] <- worker.current

# Calculate the model
model.ds <- train(training.raw.ds, cache = FALSE, root.dir,wof)

# Load tranining data
#testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )

# label.testing.raw.ds <- labelTestingDS(testing.raw.ds, label.attacklist.raw.ds)

#testing.raw.ds %>% filter( timestamp >= 922677515) %>% filter( timestamp <= 922677762)
#scoresAll <- scoring(model.ds,testing.raw.ds, label.testing.raw.ds, FALSE, root.dir)
#scoresAll <- scoringAggregate(model.ds,testing.raw.ds, label.testing.raw.ds, TRUE, root.dir)
