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
    
    features <- names(dataset)[offsetFeatures:ncol(dataset)]
    posfeature <- offsetFeatures
    
    for (feature in features) { 
      
      tic()
      print( paste("[",posfeature - offsetFeatures, "/",ncol(dataset)-offsetFeatures,"] Processing feature: [",feature,"]") )
      model[[feature]] <- Clusterer$new() 
      
      tmpCluster <- model[[feature]]
      
      for(index in 1:nrow(dataset)){
        value <- dataset[index,posfeature]
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

# ' This funcitons creates a label map. It says for if an testing entry is really or not an attack
labelTestingDS <- function(testing.raw.ds, label.attacklist.raw.ds) {
  
  label.testing.raw.ds <- testing.raw.ds %>% select(timestamp,ip.dst) %>% mutate( ts = trunc(timestamp / 1000000),  attack = FALSE)
  
  
  tic()
  for(index in 1:nrow(label.attacklist.raw.ds)){
    
    
    print( paste("[",index, "/",nrow(label.attacklist.raw.ds),"] Processing attack..." ))
    
    tsStart <-label.attacklist.raw.ds$starttime[index]
    tsEnd <- label.attacklist.raw.ds$endtime[index]
    ipDst <- label.attacklist.raw.ds$dstIP[index]
    
    label.testing.raw.ds <- label.testing.raw.ds %>% 
      mutate( attack = attack %>% replace( attack == FALSE & ts >= tsStart & ts <= tsEnd & ip.dst == ipDst, TRUE))
    
  }
  
  rnorm(1000,0,1)
  toc()
  
  return(label.testing.raw.ds)
  
}

#' Utility class that returns the name of the features
getAllFeatureNames  <- function() {
  return(c("dt.year","dt.mode","dt.week","dt.day","dt.type","timestamp","eth.size","eth.dstHi","eth.dstLow","eth.srcHi","eth.srcLow","eth.type","ip.ihl","ip.tos","ip.length","ip.id","ip.offset","ip.ttl","ip.proto","ip.chksum","ip.src","ip.dst","icmp.type","icmp.code","icmp.chksum","tcp.sport","tcp.dport","tcp.seqNo","tcp.ackNo","tcp.dataOffset","tcp.flags","tcp.window","tcp.chksum","tcp.urgPtr","tcp.options","udp.sport","udp.dport","udp.length","udp.chksum") )
  
}


#' @param model.ds             Model used to decide whether a register is an attack or not 
#' @param label.testing.raw.ds Optional. Labeled attacks on testing dataset
#' @param testing.raw.ds       Data set for evaluation. Decide if some packet is an attack or not
#' @param cache                [TODO] Do not do any calculation, use precalculated data
#' @param root.dir             [TODO] Workspace root directory used to load cached data
scoring <- function(model.ds,testing.raw.ds, label.testing.raw.ds, cache = TRUE, root.dir) {
  
  ## Only for a distrbuted execution 
  # Take information about distributed execution
  wof <- getWorkerRepartition()
  
  # Split task taking into account the workers
  if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
    worker.all <- as.numeric(Sys.getenv("worker_all"))
    worker.current <- as.numeric(Sys.getenv("worker_current"))  
  }
  
  tic(paste("Iniciando cálculo de scoring en worker ", worker.current ))
  
  ## End distributed execution
  
  allFeaturesNames <- getAllFeatureNames() 
  
  # Constants depending on columns of the dataset
  offsetFeatures <- 6
  timestampCol <- 6
  
  featureNames <- allFeaturesNames[(offsetFeatures+1):length(allFeaturesNames)]
  nfeatures <- length(featureNames)
  
  scoreNFeatures <- nfeatures
  scoreNRows <- nrow(testing.raw.ds)
  
  # Step 1)
  # Initialize last anomaly time to 1 sec before time of first packet.
  # The first column of testData is the timestamp.
  # TODO: lastAnomaly = {key: testData[0][0] - 1 for key in FEATURES}
  # TODO: Set initial timestamp for each feature according to most recent pacakge.
  initialTs <- testing.raw.ds[1,timestampCol] -1
  lastAnomaly <- data.frame(matrix(rep(initialTs,scoreNFeatures), nrow=1, ncol = scoreNFeatures))
  names(lastAnomaly) <- featureNames
  
  
  # Step 2)
  ## TODO: nr = {key: (clusters[key].getTotal(), clusters[key].getDistinct()) for key in FEATURES}
  # TODO: Repasar si es correcto
  nr <- rep(0,nfeatures)
  for(index in 1:nfeatures) {
    # Note: model.ds.cahce can be accesses as associative array: ie. model.ds.cache$eth.size, instead of using numerical index
    nr[index] <- model.ds[[index]]$getTotal()/model.ds[[index]]$getDistinct() 
  }
  
  # Step 3)
  # Create a score dataset only with features to evaluate. Information related to week, day, tstamp are not inside this dataset
  scoresAll <- data.frame(matrix(rep(0,scoreNFeatures*scoreNRows,nfeatures),nrow = scoreNRows, ncol=scoreNFeatures))
  names(scoresAll) <- featureNames
  
  #testFeature <- 17
  
  # Step 4) Calculate scoring
  # for(indexFeature in testFeature:scoreNFeatures) {
  for(indexFeature in 1:scoreNFeatures) {
    
    tic(msg = paste("Processing: [",indexFeature,"/",scoreNFeatures,"]",featureNames[indexFeature]))
    
    # Initialize information for the process
    indexFeatureOnModel <- indexFeature
    indexFeatureOnTraining <- indexFeature + offsetFeatures
    indexTimestampFeature  <- timestampCol
    
    ## Only to test
    #wof[indexFeatureOnModel] <- 3
    
    if(wof[indexFeatureOnModel] == worker.current ) {
      
      if( cache ) {
        score <- loadCacheScoringFeature(indexFeatureOnModel, root.dir)
      } 
      
      
      if( is.null(score)  ) {
        score <- scoreFeature(model.ds,testing.raw.ds, 
                              indexFeatureOnModel, indexFeatureOnTraining, indexTimestampFeature,
                              lastAnomaly[1, indexFeatureOnModel], nr)
        # Save score
        saveCacheScoringFeature(score, indexFeatureOnModel, root.dir)
      }
      
      # Update current feature on scores
      scoresAll[,indexFeature] <- score;
      
      toc()
      
    }
    
  }
  
  # Accumulate scoring
  # Score the packet and store as last element
  # scores[:, -1] = np.sum(scores[:, 0:-1], axis=1)
  #### ACTIVATE 
  #####scoresAll %>% mutate(totalScore = rowSums(.[1:33]))
  
  # If the total score of the packet is very small, set it to one so
  # that the resulting normalization doesn't fail.
  # scores[:, -1][scores[:, -1] < 1] = 1
  #### ACTIVATE scores %>% mutate(totalScore = ifelse(totalScore < 1, 1, totalScore))
  
  #results = np.hstack((testData, scores))
  #np.save(open("data/phad_results.npy", "wb"), results)
  
  print(paste("Fin scoreing: ",Sys.time()))
  toc()
  return(scoresAll)
  
}


# Calculare score for a specific feature
#' @param model.ds               Model used to decide whether a register is an attack or not 
#' @param testing.raw.ds         Data set for evaluation. Decide if some packet is an attack or not
#' @param indexFeatureOnModel    Position (column) of the current feature in the model
#' @param indexFeatureOnTraining Position (column) of the current feature in the training dataset
#' @param indexTimestampFeature  Position (column) of the timestamp feature in the training dataset
#' @param initialTimestamp       Initial timestamp in order to calculate the time since last anomaly
#' @param cache                  [TODO] Do not do any calculation, use precalculated data
#' @param root.dir               [TODO] Workspace root directory used to load cached data
scoreFeature <- function(model.ds,testing.raw.ds, indexFeatureOnModel, indexFeatureOnTraining, indexTimestampFeature, initialTimestamp, nr) {
  
  # Initialize lastAnomaly timestamp
  lastAnomaly <-initialTimestamp
  
  # Get num of observations
  scoreNRows <- nrow(testing.raw.ds)
  
  # Initialize score feature vector to 0
  scores <- rep(0,scoreNRows)
  
  for(indexObs in 1:scoreNRows) {
    
    valueFeature <- testing.raw.ds[indexObs,indexFeatureOnTraining]
    
    # Check if current feature doesn't not apply to the model
    # The feature has not to be taken into an account if the value is NA
    if(! is.na(valueFeature ) ) { 
      
      if( model.ds[[indexFeatureOnModel]]$contains(valueFeature) ) {
        # Do nothing. It is a valid value for that feature
      } else {
        
        # Detected possible anomaly, the score has to be calculated
        timestamp <- testing.raw.ds[indexObs,indexTimestampFeature]
        t = timestamp - lastAnomaly
        
        # Calculate new score
        scores[indexObs] = t * nr[indexFeatureOnModel]
        
        # Update current timestamp
        lastAnomaly = timestamp
        
      }
      
    }
    
  }
  
  return(scores)
}
