library("tidyr")


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

loadCacheTrainingRaw <- function(root.dir=getwd(), forceRemote = TRUE) {

  file.name <- 'training.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else if( forceRemote ) {
    return(getCacheTrainingRawFromURL())
  } else {
    return(NULL)
  }
  
  return(training.raw.ds.cache)
  
}

loadCacheTestingRaw <- function(root.dir=getwd(), forceRemote = TRUE) {
  
  file.name <- 'testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else if( forceRemote ) {
    return(getCacheTestingRawFromURL())
  } else {
    return(NULL)
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

loadCacheLabelAttackTypesRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.attacktypes.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(label.attacktypes.raw.ds.cache)
  
}



loadCacheLabelTestingRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(label.testing.raw.ds.cache)
  
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

loadCacheTestingDatasetScored <- function(root.dir= getwd()) {
  
  
  file.name <- 'testing.raw.scored.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if(file.exists(file.fullname)) {
    load(file = file.fullname)
  } else {
    return(NULL);
  }
  
  return(testing.raw.scored.ds.cache)
  
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

saveCacheTestingDatasetScored <- function(root.dir= getwd()) {
  
  file.name <- 'testing.raw.scored.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  testing.raw.scored.ds.cache <- testing.raw.scored.ds
  
  save(testing.raw.scored.ds.cache,file = file.fullname )
  
}

saveCacheModel <- function(root.dir= getwd()) {
  
  file.name <- 'model.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  model.ds.cache <- model.ds
  
  save(model.ds.cache,file = file.fullname )
  
}


saveCacheLabelAttackTypeRaw <- function(root.dir=getwd(), label.attacktypes.raw.ds.cache = NULL) {
  
  file.name <- 'label.attacktypes.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if( is.null( label.attacktypes.raw.ds.cache ) ) {
    label.attacktypes.raw.ds.cache <- label.attacktypes.raw.ds
  }
  
  save(label.attacktypes.raw.ds.cache,file = file.fullname )
  
}

saveCacheLabelAttackListRaw <- function(root.dir=getwd(), label.attacklist.raw.ds.cache = NULL) {
  
  file.name <- 'label.attacklist.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  if( is.null(label.attacklist.raw.ds.cache)) {
    label.attacklist.raw.ds.cache <- label.attacklist.raw.ds
  }
  
  save(label.attacklist.raw.ds.cache,file = file.fullname )
  
}

saveCacheLabelTestingRaw <- function(root.dir=getwd()) {
  
  file.name <- 'label.testing.raw.ds.cache.RData'
  file.fullname <- paste( getCacheDir(root.dir),file.name,sep="/")
  
  label.testing.raw.ds.cache <- label.testing.raw.ds
  
  save(label.testing.raw.ds.cache,file = file.fullname )
  
}


