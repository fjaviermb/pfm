# This class implements PHad algorithm using clusters of 32 elements
# Benchmarking: https://stackoverflow.com/questions/6262203/measuring-function-execution-time-in-r
require(tictoc)
source(paste(root.dir,"cache/cacheLoader.R",sep="/"))
source(paste(root.dir,"input/inputLoader.R",sep="/"))
source(paste(root.dir,"util/clusterer.R",sep="/"))


#' @param cache     Do not do any calculation, use precalculated data
#' @param root.dir  Workspace root directory used to load cached data
train <- function(dataset, cache = FALSE, root.dir=getwd()) {
  
  model = list()

  if( cache ) {
    
    model = loadCacheModel(root.dir)
  
  } else {
    
    offsetFeatures = 7
      
    features <- names(dataset)[offsetFeatures:ncol(dataset.entries)]
    posfeature <- offsetFeatures
    
    for (feature in features) { 
  
      tic()
      print( paste("[",posfeature - offsetFeatures, "/",ncol(dataset.entries)-offsetFeatures,"] Processing feature: [",feature,"]") )
      model[[feature]] <- Clusterer$new() 
      
      tmpCluster <- model[[feature]]
      
      for(index in 1:nrow(dataset)){
        value <- dataset.entries[index,posfeature]
        if( !  is.na(value) ) {
          tmpCluster$add(value)
        }
        
      }
      
      rnorm(1000,0,1)
      toc()
      
      posfeature <- posfeature + 1
  
    }
  } 
  
  return(model)
  
}

scoring <- function() {
  
  # TODO
  
}