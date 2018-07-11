library("dplyr")
library("tidyr")
library("ggplot2")
library("grid")
library("gridExtra")

source(paste(getwd(),'code/util/environment.R', sep="/"))

# Clean environment and do environment setup tasks
setup()

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

evaluateModel <- function(model.ds, root.dir = getwd(), cache = TRUE, wof = NULL) {
  
  # Load testing dataset
  testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )
  logger("Testing data loaded")
  
  # Score testing dataset
  testing.raw.scored.ds <- scoring(model.ds,testing.raw.ds, root.dir, TRUE, wof )
  logger("Testing data scored")
  
  # Sum all score and do normalisation
  testing.raw.scored.ds <- getScoredTestingData(testing.raw.scored.ds, cache = TRUE, root.dir);
  
  return(testing.raw.scored.ds)
  
}

measureResults <- function(testing.raw.scored.ds, root.dir = getwd(), cache = TRUE) {

  # Load master attack type list
  label.attacktypes.raw.ds <- loadLabelAttackTypes(cache = FALSE, root.dir)
  logger("Label attack type list loaded")
  
  # Load master attack list
  label.attacklist.raw.ds <- loadLabelAttackList(cache = FALSE, root.dir)
  logger("Label attack list loaded")
  
  # At the end, we only use log scale, ScoringMin and ScoringNormMin were an inital solution
  testing.measured.ds <- testing.raw.scored.ds %>% select(scoringLog, scoringNormLog) %>% mutate ( scoring = scoringLog, scoringNorm = scoringNormLog)
 
  # Load testing data to measure results
  testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )
  
  # Get a labeled attack list
  label.testing.raw.ds <- labelTestingDS(testing.raw.ds, label.attacklist.raw.ds, label.attacktypes.raw.ds, cache = TRUE, root.dir)
  
  logger("Label data loaded")
  rm(testing.raw.ds)
  gc()
  
  # Label current testing scored data
  testing.measured.ds <- getLabeledTestingData(testing.measured.ds, label.testing.raw.ds)
  rm(label.testing.raw.ds)
  gc()
  
  return(testing.measured.ds)
  

}

logger("Calculate model")
model.ds <- calculateModel(root.dir, cache = TRUE, wof = NULL)

logger("Evaluate model")
wof <- rep(-1,33)

testing.raw.scored.ds <- evaluateModel(model.ds, root.dir, cache = TRUE, wof = NULL)
logger("Measure results")
testing.measured.ds <- measureResults(testing.raw.scored.ds, root.dir, cache)
rm(testing.raw.scored.ds)
gc()


