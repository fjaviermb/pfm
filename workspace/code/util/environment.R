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
# Common installation of ggplot2 (with error):
#  1) install.packages("tidyverse")
#  2) install.packages("ggplot2")
# Current installation of ggplot2 (working):
#  devtools::install_github("tidyverse/ggplot2")
# install.packages("gridExtra")

setup <- function(env = parent.frame() ) {
  
  # Clear environment
  rm(list=ls(envir = env), envir = env)
  
  # Should be the "code" folder
  env$root.dir <- paste(getwd(),"code", sep="/")
  
  #https://stackoverflow.com/questions/12642651/in-r-how-to-get-error-messages-in-english
  ## English messages
  Sys.setlocale("LC_MESSAGES", "C")
  
  #https://stackoverflow.com/questions/9397664/force-r-not-to-use-exponential-notation-e-g-e10
  ## Avoid scientific representation. This option aids to print R command to display a number in scientific/exponential representation or not
  options("scipen"=100, "digits"=4)

  # Load scripts
  source(paste(root.dir,"util/environment.R",sep="/"))
  source(paste(root.dir,"util/distribution.R",sep="/"))
  source(paste(root.dir,"input/inputLoader.R",sep="/"))
  source(paste(root.dir,"util/clusterer.R",sep="/"))
  source(paste(root.dir,"phad-c32.R",sep="/"))  
 
}

getRawDir <- function(root.dir = getwd()) {
  
  return( paste(root.dir,"..","raw",sep="/"))
  
}

logger <- function(message) {
  print(paste("[",Sys.time(),"][",getWorker(),"] ", message, sep=""))
}


