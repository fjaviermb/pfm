library("tidyr")

getCacheDir <- function(root.dir = getwd()) {
  return( paste(root.dir,"cache",sep="/"))
}

#' Maybe deprecated
#' Usage:  
## Only for a distrbuted execution 
# Take information about distributed execution
#wof <- getWorkerRepartition()
getWorkerRepartition <- function() {
  
  wof<- rep(0,33)
  wof[1]<-2
  wof[2]<-4
  wof[3]<-3
  wof[4]<-0
  wof[5]<-1
  wof[6]<-5
  wof[7]<-5
  wof[8]<-0
  wof[9]<-3
  wof[10]<-5
  wof[11]<-5
  wof[12]<-0
  wof[13]<-1
  wof[14]<-4
  wof[15]<-2
  wof[16]<-1
  wof[17]<-1
  wof[18]<-1
  wof[19]<-5
  wof[20]<-3
  wof[21]<-1
  wof[22]<-4
  wof[23]<-1
  wof[24]<-3
  wof[25]<-1
  wof[26]<-5
  wof[27]<-4
  wof[28]<-0
  wof[29]<-0
  wof[30]<-0
  wof[31]<-5
  wof[32]<-0
  wof[33]<-0
  
  return(wof)
  
}


loadCacheModel <- function(root.dir= getwd()) {
  
  file.name <- 'model.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(model.ds.cache)
  
}

loadCacheTrainingCluster <- function(root.dir= getwd()) {
  
  file.name <- 'trainig.cluster.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(training.cluster.ds.cache)
  
}

loadCacheTrainingRaw <- function(root.dir=getwd()) {
  
  file.name <- 'training.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(training.raw.ds.cache)
  
}

loadCacheTestingRaw <- function(root.dir=getwd()) {
  
  file.name <- 'testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(testing.raw.ds.cache)
  
}

loadCacheLabelAttackListRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.attacklist.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(label.attacklist.raw.ds.cache)
  
}


loadCacheScoringFeature <- function(nFeature, root.dir= getwd()) {
  
  file.name <- paste('scoring.feature',nFeature,'.ds.cache.RData', sep = "")
  file.fullname <- paste( getCacheDir(root.dir),'partial',file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    return(readRDS(file = file.fullname))
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
}

saveCacheModelFeature <- function(object, nFeature, root.dir= getwd()) {
  
  file.name <- paste('model.feature',nFeature,'.ds.cache.RData', sep = "")
  file.fullname <- paste( getCacheDir(root.dir),'partial',file.name,sep="/")
  
  saveRDS(object,file = file.fullname)
  
}
saveCacheScoringFeature <- function(object, nFeature, root.dir= getwd()) {
  
  file.name <- paste('scoring.feature',nFeature,'.ds.cache.RData', sep = "")
  file.fullname <- paste( getCacheDir(root.dir),'partial',file.name,sep="/")
  
  saveRDS(object,file = file.fullname)
  
}

loadCacheModelFeature <- function(nFeature, root.dir= getwd()) {
  
  file.name <- paste('model.feature',nFeature,'.ds.cache.RData', sep = "")
  file.fullname <- paste( getCacheDir(root.dir),'partial',file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    return(readRDS(file = file.fullname))
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
}


saveCache <- function(object, filename, root.dir=getwd()) {
  save(object, file =  paste( getCacheDir(root.dir), filename ,sep="/") )
  save(object, file =  paste( getCacheDir(root.dir), 'partial', filename ,sep="/") )
}


saveCacheTraniningDataset <- function(root.dir= getwd()) {
  
  file.name <- 'training.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  training.raw.ds.cache <- training.raw.ds
  
  save(training.raw.ds.cache,file = file.fullname )
  
}


saveCacheTestingDataset <- function(root.dir= getwd()) {
  
  file.name <- 'testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  testing.raw.ds.cache <- testing.raw.ds
  
  save(testing.raw.ds.cache,file = file.fullname )
  
}

saveCacheModel <- function(root.dir= getwd()) {
  
  file.name <- 'model.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  model.ds.cache <- model.ds
  
  save(model.ds.cache,file = file.fullname )
  
}


saveCacheLabelAttackListRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.attacklist.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  label.attacklist.raw.ds.cache <- label.attacklist.raw.ds
  
  save(label.attacklist.raw.ds.cache,file = file.fullname )
  
}

saveCacheLabelTestingRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  label.testing.raw.ds.cache <- label.testing.raw.ds
  
  save(abel.testing.raw.ds.cache,file = file.fullname )
  
}

label.testing.raw.ds 

