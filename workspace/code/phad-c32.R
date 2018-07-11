# Implements PHad algorithm using clusters of 32 elements

# Benchmarking: https://stackoverflow.com/questions/6262203/measuring-function-execution-time-in-r
require(tictoc)

source(paste(root.dir,"cache/cacheLoader.R",sep="/"))
source(paste(root.dir,"input/inputLoader.R",sep="/"))
source(paste(root.dir,"util/clusterer.R",sep="/"))



#' @param cache     Do not do any calculation, use precalculated data
#' @param root.dir  Workspace root directory used to load cached data
#' @param wof       Indicates which feature is going to calculate
train <- function(dataset, cache = FALSE, root.dir=getwd(), wof=NULL) {
  
  # Just for distributed execution
  worker.current <- 0
  # Split task taking into account the workers
  if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
    worker.all <- as.numeric(Sys.getenv("worker_all"))
    worker.current <- as.numeric(Sys.getenv("worker_current"))  
  }
  
  if( is.null(wof) ) {
    wof<- rep(worker.current,33) 
  }
  
  model = list()
  
  
  # If cache is enabled, try to load. If it is not available, continue with cache for the objects/parts of the model
  if( cache ) {
    
    model = loadCacheModel(root.dir)
    
  } 
  
  if( ! cache || is.null(model) ) {
    
    offsetFeatures = 7
    
    features <- names(dataset)[offsetFeatures:ncol(dataset)]
    posfeature <- offsetFeatures
    
    for (feature in features) { 
      
      tic()
      
      nFeature = posfeature - offsetFeatures + 1;
      
      logger( paste("[",nFeature, "/",ncol(dataset)-offsetFeatures+1,"] Processing feature: [",feature,"]") )
      
      featureObs <- dataset[,posfeature]
      
      # Calculate only for this worker
      
      if(wof[nFeature] == worker.current ) {
        
        model[[feature]] <- trainFeature (featureObs, nFeature, cache, root.dir)
        
        rnorm(1000,0,1)
        
        toc()
        
      } else {
        
        # Init with empty information
        model[[feature]] <- Clusterer$new()
        
      }
      
      posfeature <- posfeature + 1
      
    }
  } 
  
  return(model)
  
}


#' Specific feature training
#' @param featureObs  Observations for current feature (vector)
#' @param nFeature    Feature position in order to save or load from cache 
#' @param cache       Do not do any calculation, use precalculated data
#' @param root.dir    Workspace root directory used to load cached data
trainFeature <-  function(featureObs, nFeature, cache = FALSE, root.dir = getwd()) {
  
  if (cache) {
    
    model = loadCacheModelFeature(nFeature, root.dir)
    
  } else {
    model = Clusterer$new()
    
    for (index in 1:length(featureObs)) {
      featureObservation <- featureObs[index]
      if (!is.na(featureObservation)) {
        model$add(featureObservation)
      }
      
    }
    
    # Save calculated model
    saveCacheModelFeature(model, nFeature, root.dir);
    
  }
  
  return(model)
  
}

# ' This funcitons creates a label map. It says for if an testing entry is really or not an attack
#' @param cache       Do not do any calculation, use precalculated data
#' @param root.dir    Workspace root directory used to load cached data
labelTestingDS <- function(testing.raw.ds, label.attacklist.raw.ds, label.attacktypes.raw.ds, cache = FALSE, root.dir = getwd()) {
  
  if( cache ) {
    
    label.testing.raw.ds <- loadCacheLabelTestingRaw(root.dir)
    
  }
  
  if( !cache || is.null(label.testing.raw.ds)) {

    # Generate attack column: is or not an attack    
    label.testing.raw.ds <- testing.raw.ds %>% select(timestamp,ip.dst) %>% mutate( ts = timestamp,  attack = FALSE, name = NA)
    
    tic()
    nrows <- nrow(label.attacklist.raw.ds)
    for(index in 1:nrows){
      
      
      logger( paste("[",index, "/",nrows, "] Processing attack..." ))
      
      tsStart <-label.attacklist.raw.ds$starttime[index]
      tsEnd <- label.attacklist.raw.ds$endtime[index]
      ipDst <- label.attacklist.raw.ds$dstIP[index]
      name <- label.attacklist.raw.ds$name[index]
      
      label.testing.raw.ds <- label.testing.raw.ds %>% 
        mutate( attack = attack %>% replace( attack == FALSE & ts >= tsStart & ts <= tsEnd & ip.dst == ipDst, TRUE) )
      
    }
    
    # Generate name column: the name of the attack
    nrows <- nrow( label.testing.raw.ds)
    for(index in 1:nrows){
      
      logger( paste("[",index, "/",nrows,"] Getting attack name, attack id..." ))
      
      if( isTRUE(label.testing.raw.ds[['attack']][index])) {
        
        ts <- label.testing.raw.ds[['ts']][index] 
        ipDst <- label.testing.raw.ds[['ip.dst']][index] 
        
        attackName <- first(label.attacklist.raw.ds %>% filter( starttime <= ts & endtime >= ts & dstIP == ipDst ) %>% select (name))
        label.testing.raw.ds[['name']][index] <- attackName
        
      }
      
    }
    
    # Generate attackid column: the id of the attack
    label.testing.raw.ds <- enrichAttackIdLabelTestingDS(label.testing.raw.ds, label.attacklist.raw.ds)
    
    # Generate type column: the attack type of the attack
    
    # Drop columns if already exists
    if( 'type' %in% names(label.testing.raw.ds) ) {
      label.testing.raw.ds <- label.testing.raw.ds %>% select (-type) 
    }
    
    label.testing.raw.ds <- label.testing.raw.ds %>% left_join(
      label.attacktypes.raw.ds, by=c('name') )
    
    rnorm(1000,0,1)
    toc()
    
  }
  
  return(label.testing.raw.ds)
  
  
}



# ' Adds attack id information
enrichAttackIdLabelTestingDS <- function(label.testing.raw.ds, label.attacklist.raw.ds) {
  
  # init attackid column
  label.testing.raw.ds <- label.testing.raw.ds %>% mutate(attackid = NA)
  
  # get end condition when all attacklist has been processed
  nattacks <- length(label.attacklist.raw.ds)
  
  # Just for showing information for long operations
  logrange <- 2000
  
  nrows <- nrow( label.testing.raw.ds)
  for(index in 1:nrows){
    
    if( index %% logrange == 0) {
      logger( paste("[",index, "/",nrows,"] Getting attack id..." ))
    }
    
    if( isTRUE(label.testing.raw.ds[['attack']][index])) {
      
      ts <- label.testing.raw.ds[['ts']][index] 
      ipDst <- label.testing.raw.ds[['ip.dst']][index] 
      
      attackid <- first(label.attacklist.raw.ds %>% filter( starttime <= ts & endtime >= ts & dstIP == ipDst ) %>% select (id))
      label.testing.raw.ds[['attackid']][index] <- attackid
      
    }
    
    
    
  }
  
  return(label.testing.raw.ds)
  
}


#' @param model.ds             Model used to decide whether a register is an attack or not 
#' @param testing.raw.ds       Data set for evaluation. Decide if some packet is an attack or not
#' @param cache                Do not do any calculation, use precalculated data
#' @param root.dir             Workspace root directory used to load cached data
#' @param wof       Indicates which feature is going to calculate
scoring <- function(model.ds, testing.raw.ds, root.dir = getwd(),  cache = TRUE, wof = NULL) {
  
  tic("Starting scoring testing data")
  
  # Step 1: select only required fields
  testing.timestamps.ds <- testing.raw.ds %>% select(timestamp)
  testing.features.ds <- testing.raw.ds %>% select( -dt.year, -dt.mode, -dt.week, -dt.day, -dt.type, -timestamp )
  
  nfeatures <- length(testing.features.ds)
  
  # Step 2: initialize last anomaly time to 1 sec before time of first packet.
  initialTs <- testing.timestamps.ds[1,1]-1
  
  # Step 3: Create score dataframe
  testing.scored.ds <- data.frame(
    matrix(rep(0,dim(testing.features.ds)[1]*dim(testing.features.ds)[2],nfeatures),
           nrow = dim(testing.features.ds)[1], ncol=dim(testing.features.ds)[2]))
  names(testing.scored.ds) <- names(testing.features.ds)
  
  # Step 4: Precalculate n/r factor
  nr <- sapply(  
    seq(nfeatures), 
    function(index, model.ds) { return (model.ds[[index]]$getTotal()/model.ds[[index]]$getDistinct()) },
    model.ds = model.ds)
  
  # Step 5: Calculate scoring
  for(indexFeature in 1:nfeatures) {
    
    tic(msg = paste("Processing: [",indexFeature,"/",nfeatures,"]",names(testing.features.ds)[indexFeature]))
    
    # Initialize score to NULL
    testing.score.feature.ds <- NULL
    
    logger(paste("Processing en scoring ",indexFeature))
    
    if( is.null(wof) ||  ( !is.null(wof) && wof[indexFeature] == getWorker() ) ) {
      
      if( cache ) {
        testing.score.feature.ds <- loadCacheScoringFeature(indexFeature, root.dir)
      } 
      
      if( ! exists("testing.score.feature.ds") || is.null(testing.score.feature.ds)  ) {
        
        testing.score.feature.ds <- scoreFeature( 
          model.ds, 
          testing.features.ds[indexFeature], testing.timestamps.ds, 
          nr[indexFeature],  indexFeature,  initialTs) 
        
        # Save score
        saveCacheScoringFeature(testing.score.feature.ds, indexFeature, root.dir)
      }
      
      # Update current feature on scores
      testing.scored.ds[,indexFeature] <- testing.score.feature.ds;
      
      toc()
      
    }
    
  }
  
  logger(paste("Fin scoring: ",Sys.time()))
  toc()
  return(testing.scored.ds)
}


# Calculare score for a specific feature
#' @param model.ds               Model used to decide whether a register is an attack or not 
#' @param testing.features.ds    Data set for evaluation. Decide if some packet is an attack or not
#' @param testing.timestamps.ds  Data set with timestamps.
#' @param indexFeature           Position (column) of the current feature
#' @param initialTs              Initial timestamp in order to calculate the time since last anomaly
scoreFeature <- function( model.ds,  testing.features.ds, testing.timestamps.ds,  nr,  indexFeature,  initialTs  ) {
  
  # Initialize lastAnomaly timestamp
  lastAnomaly <-initialTs
  
  # Get num of observations
  scoreNRows <- nrow(testing.features.ds)
  
  # Initialize score feature vector to 0
  scores <- rep(0,scoreNRows)
  
  for(indexObs in 1:scoreNRows) {
    
    valueFeature <- testing.features.ds[[1]][indexObs]
    
    # Check if current feature doesn't not apply to the model
    # The feature has not to be taken into an account if the value is NA
    if(! is.na(valueFeature ) ) { 
      
      if( ! model.ds[[indexFeature]]$contains(valueFeature) ) {
        
        # Detected possible anomaly, the score has to be calculated
        timestamp <- testing.timestamps.ds[[1]][indexObs]
        t = timestamp - lastAnomaly
        # Calculate new score
        scores[indexObs] = t * nr
        # Update current timestamp
        lastAnomaly = timestamp
        
      }
    }
  }
  
  return(scores)
}



# 'This functions is only for results validation. Create a dataframe with the following information from model
# ' Fieldname: feature
# ' r/n: text containrin r value + character "/" + n Value
# ' r: value of r
# ' n: value of n
# ' Values: first two values
#' @param model.ds             Model used to decide whether a register is an attack or not 
getModelDataframe <- function(model.ds) 
{
  
  featureNames <-  c("Field Name","r/n","r","n","Values");
  
  nrow <-  length(model.ds)
  ncol <- length(featureNames)
  
  
  training.tabular.model.ds <- data.frame(matrix(rep(0,nrow,ncol),nrow=nrow,ncol=ncol))
  trainingFeatureNames <- names(model.ds)
  
  
  
  for(nFeature in 1:nrow) {
    
    logger(nFeature)
    
    clusterFeature <- model.ds[[nFeature]];
    
    training.tabular.model.ds[nFeature,1] <-  trainingFeatureNames[nFeature]
    training.tabular.model.ds[nFeature,3] <-  clusterFeature$getDistinct()
    training.tabular.model.ds[nFeature,4] <-  clusterFeature$getTotal()
    training.tabular.model.ds[nFeature,2] <-  paste(training.tabular.model.ds[nFeature,3],'/',training.tabular.model.ds[nFeature,4])
    training.tabular.model.ds[nFeature,5] <-  paste(
      paste(clusterFeature$getClusters()[[2]][1],'-',clusterFeature$getClusters()[[3]][1],sep=''), 
      paste(clusterFeature$getClusters()[[2]][2],'-',clusterFeature$getClusters()[[3]][2],sep=''),
      paste(clusterFeature$getClusters()[[2]][3],'-',clusterFeature$getClusters()[[3]][3],sep=''),
      sep=' ')
  }
  
  names(training.tabular.model.ds) <- featureNames;
  
  return(training.tabular.model.ds)
  
  
}


getLabeledTestingData <- function(testing.raw.ds, label.testing.raw.ds) {
  
  # Drop columns if already exists
  if( 'attackid' %in% names(testing.raw.ds) ) {
   testing.raw.ds <- testing.raw.ds %>% select (-attackid) 
  }
  
  if( 'attack' %in% names(testing.raw.ds) ) {
    testing.raw.ds <- testing.raw.ds %>% select (-attack) 
  }

  if( 'name' %in% names(testing.raw.ds) ) {
    testing.raw.ds <- testing.raw.ds %>% select (-name) 
  }

  if( 'type' %in% names(testing.raw.ds) ) {
    testing.raw.ds <- testing.raw.ds %>% select (-type) 
  }
  
  testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['attackid'])
  testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['attack'])
  testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['name'])
  testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['type'])
  
  return(testing.raw.ds)
  
}



#' Temporary function to load off scores from cache
getScoredTestingData <- function(testing.scored.ds, cache = TRUE, root.dir) {
  
  # Scoring column: accumulates scoring for all the features for the each observation
  testing.scored.ds <- testing.scored.ds %>% mutate(scoring = rowSums(.[1:33]))
  # ScoringMin column: Values below 0 move to value 1 to use log10n normalisation and not having problems.
  testing.scored.ds <-  testing.scored.ds %>% mutate(scoringMin = ifelse(scoring < 1, 1, scoring))
  # ScoringLog column: do log10 transformation
  testing.scored.ds <-  testing.scored.ds %>% mutate(scoringLog = log10(scoringMin))
  
  # Normalisation: Rescale between 1 and 100 to apply threshold for the log and the min
  testing.scored.ds <-  testing.scored.ds %>% mutate ( scoringNormMin = lapply( testing.scored.ds['scoringMin'], minMaxScaler )[[1]])
  testing.scored.ds <-  testing.scored.ds %>% mutate ( scoringNormLog = lapply( testing.scored.ds['scoringLog'], minMaxScaler )[[1]])
  
  return(testing.scored.ds)
  
}



#' Do MinMax scaler/normalizacion
# x: data. numeric vector of values to be scaled
# ' minVal: minimum value
# ' maxVal: maximum value
# based on: https://gist.github.com/swayson/b5a6d3cd796ab1d08df1
minMaxScaler <- function(x, minVal=1, maxVal=100) {
  return ( (((maxVal-minVal)*(x - min(x))) / (max(x) - min(x))) + minVal )
}

