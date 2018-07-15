library("dplyr")
library("tidyr")
library("ggplot2")
library("grid")
library("gridExtra")

source(paste(getwd(),'R/util/environment.R', sep="/"))

# Clean environment and do environment setup tasks
setup()

#' Build the model with the input data
#'
#' @param cache     Do not do any calculation, use precalculated data if it is available
#' @param root.dir  Workspace root directory used to load cached data
#' @param wof       If it is not NULL, indicates which feature is going to calculate by current computer.
#' @return the trained model
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

#' Evaluates the model with the input data (score each packet)
#'
#' @model.ds        Trained model
#' @param cache     Do not do any calculation, use precalculated data if it is available
#' @param root.dir  Workspace root directory used to load cached data
#' @param wof       If it is not NULL, indicates which feature is going to calculate by current computer.
#' @return the trained model
evaluateModel <- function(model.ds, root.dir = getwd(), cache = TRUE, wof = NULL) {

  # Load testing dataset
  testing.raw.ds <- loadTestingDataset(cache = TRUE, root.dir )
  logger("Testing data loaded")

  # Score testing dataset
  testing.raw.scored.ds <- scoring(model.ds,testing.raw.ds, root.dir, TRUE, wof )
  logger("Testing data scored")

  # Sum all score and do normalisation
  testing.raw.scored.ds <- getScoredTestingData(testing.raw.scored.ds);

  return(testing.raw.scored.ds)

}

#' Extract measures from the evaluation results
#'
#' @testing.raw.scored.ds  Testing data with the scoring
#' @param cache     Do not do any calculation, use precalculated data if it is available
#' @param root.dir  Workspace root directory used to load cached data
#' @param wof       If it is not NULL, indicates which feature is going to calculate by current computer.
#' @return the trained model
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

# Sequence example
#logger("Calculate model")
#model.ds <- calculateModel(root.dir, cache = TRUE, wof = NULL)
#logger("Evaluate model")
#testing.raw.scored.ds <- evaluateModel(model.ds, root.dir, cache = TRUE, wof = NULL)
#logger("Measure results")
#testing.measured.ds <- measureResults(testing.raw.scored.ds, root.dir, cache)
#rm(testing.raw.scored.ds)
#gc()



