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

root.dir <- "~/git-repos/pfm/workspace/code"
#https://stackoverflow.com/questions/12642651/in-r-how-to-get-error-messages-in-english
## English messages
Sys.setlocale("LC_MESSAGES", "C")

# Load scripts
source(paste(root.dir,"input/inputLoader.R",sep="/"))
source(paste(root.dir,"util/clusterer.R",sep="/"))
source(paste(root.dir,"phad-c32.R",sep="/"))

# Saving cache
# root.dir <- "~/git-repos/pfm/workspace/code"
# input.dir <- paste(root.dir,'cache',sep='/')

# model.ds.cache <- model.ds
# file.name <- 'model.ds.cache.RData'
# file.fullname <- paste( input.dir,file.name,sep="/")
# save(model.ds.cache,file = file.fullname)

# training.raw.ds.cache <- dataset.entries
# file.name <- 'training.raw.ds.cache.RData'
# file.fullname <- paste( input.dir,file.name,sep="/")
# save(training.raw.ds.cache,file = file.fullname)

# testing.raw.ds.cache <- testing.raw.ds
# file.name <- 'testing.raw.ds.cache.RData'
# file.fullname <- paste( input.dir,file.name,sep="/")
# save(testing.raw.ds.cache,file = file.fullname)

# Load tranining data
training.raw.ds <- loadTrainingDataset(cache = TRUE, root.dir)

# Calculate model
model.ds <- train(dataset.entries, cache = TRUE, root.dir)

# Load tranining data
testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )

