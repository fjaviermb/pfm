library("tidyr")

loadCacheModel <- function(root.dir= getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'model.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(model.ds.cache)
  
}

loadCacheTrainingCluster <- function(root.dir= getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'trainig.cluster.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(training.cluster.ds.cache)
  
}

loadCacheTrainingRaw <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'training.raw.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(training.raw.ds.cache)
  
}

loadCacheTestingRaw <- function(root.dir=getwd()) {
  
  input.dir <- paste(root.dir,'cache',sep='/')
  file.name <- 'testing.raw.ds.cache.RData'
  file.fullname <- paste( input.dir,file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(testing.raw.ds.cache)
  
}

