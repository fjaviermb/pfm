# Installation for ggplot2
# The easiest way to get ggplot2 is to install the whole tidyverse:
# install.packages("tidyverse")

# Alternatively, install just ggplot2:
# install.packages("ggplot2")

# Or the the development version from GitHub:
# install.packages("devtools")
# devtools::install_github("tidyverse/ggplot2")


# testing.raw.scored.ds.RData <- loadCacheTestingDatasetScored(root.dir)

#source(paste(root.dir,"util/formatter.R",sep="/"))
#source(paste(root.dir,"util/render.R",sep="/"))

getRenderFolder <- function() {
  return('/mnt/shared/spfm/memoria_graficos')
}

getFPFNByAttackTable <- function(scored.ds, th = 0) {
  
  # We analise only attacks, so filter and get only required fields
  scored.ds.reduced <- scored.ds %>% filter( attack == TRUE ) %>% select(attackid, scoringNorm)

    # Get total attacks
  totalAttacks <- getTotalAttacks(scored.ds)
  
  # Threshold used to filter packages
  thValue <- th
  
  # Falses that are OK, set value to 0, as we don't have any attack information
  foAttacks <- 0
  # True that are OK, group by attack
  toAttacks <-  scored.ds.reduced %>% filter( scoringNorm > thValue) %>% select(attackid) %>% group_by(attackid) %>% summarise(n=n())
  # False negatives, attacks detected as no anomaly, group by id
  fnAttacks <-  scored.ds.reduced %>% filter( scoringNorm <= thValue) %>% select(attackid) %>% group_by(attackid) %>% summarise(n=n())
  # False positives, set value to 0, as they are considered an attack, but indeed, they are not an atack
  fpAttacks <- 0
  
  # Remove from False negatives, attacks groups that already has detected
  fnAttacksFiltered <- anti_join(fnAttacks, toAttacks, "attackid")

  foAttacks <- foAttacks
  toAttacks <- dim(toAttacks)[[1]]
  fnAttacks <- dim(fnAttacksFiltered)[[1]]
  fpAttacks <- fpAttacks
  
  columnNames <- c('Número ataques', 'Detección Positiva', 'Detección Negativa', 'Falsos Positivos', 'Falsos Negativos')
  
  scores <- data.frame(
    matrix(rep(0,2*length(columnNames)),nrow = 2, ncol=length(columnNames)),
    stringsAsFactors = FALSE)
  
  names(scores) <- columnNames
  
  scores[1,1] <- 'Absoluto'
  scores[1,2] <- toAttacks
  scores[1,3] <- foAttacks
  scores[1,4] <- fpAttacks
  scores[1,5] <- fnAttacks
  
  scores[2,1] <- '% Porcentaje'
  scores[2,2] <- toAttacks*100/totalAttacks
  scores[2,3] <- foAttacks*100/totalAttacks
  scores[2,4] <- fpAttacks*100/totalAttacks
  scores[2,5] <- fnAttacks*100/totalAttacks
  
  return (scores)
  
}


getFPFNTable <- function(testing.raw.scored.ds, th = 0) {
  totalPackets <- dim(testing.raw.scored.ds)[[1]]
  
  testing.raw.scored.ds.reduced <- testing.raw.scored.ds %>% select(attack, scoringNorm)
  
  thValue <- th
  totalPackages <- dim(testing.raw.scored.ds)[[1]]
  foPackages <- dim(testing.raw.scored.ds %>% filter(attack == FALSE & scoringNorm <= thValue))[[1]]
  toPackages <- dim(testing.raw.scored.ds %>% filter(attack == TRUE  & scoringNorm >  thValue))[[1]]
  fnPackages <- dim(testing.raw.scored.ds %>% filter(attack == FALSE & scoringNorm >  thValue))[[1]]
  fpPackages <- dim(testing.raw.scored.ds %>% filter(attack == TRUE  & scoringNorm <= thValue))[[1]]
  
  columnNames <- c('Número paquetes', 'Detección Positiva', 'Detección Negativa', 'Falsos Positivos', 'Falsos Negativos')
  
  scores <- data.frame(
    matrix(rep(0,2*length(columnNames)),nrow = 2, ncol=length(columnNames)),
    stringsAsFactors = FALSE)
  
  names(scores) <- columnNames
  
  scores[1,1] <- 'Absoluto'
  scores[1,2] <- toPackages
  scores[1,3] <- foPackages
  scores[1,4] <- fpPackages
  scores[1,5] <- fnPackages
  
  scores[2,1] <- '% Porcentaje'
  scores[2,2] <- toPackages*100/totalPackets
  scores[2,3] <- foPackages*100/totalPackets
  scores[2,4] <- fpPackages*100/totalPackets
  scores[2,5] <- fnPackages*100/totalPackets
  
  return (scores)
  
}



getTotalAttacks <- function(scored.ds) {
  dbAttacks <- (
    scored.ds %>% filter(attack == TRUE) %>% select(attackid) %>% group_by(attackid) %>% summarise(n=n())
  );
  
  return (dim( dbAttacks )[1] )
   
}




# For each type of attack, return the number of attacks detected
getTotalAttacksByType <- function(testing.raw.scored.ds) {

  # Get for typw who many attackas names are differente
  return (
    testing.raw.scored.ds %>% filter(attack==TRUE) %>% select(attackid,type) %>% group_by(attackid, type) %>% summarise() %>% group_by(type) %>% summarise(n=n())
  )
  
}


# For a specfic type of attack, return the number of attacks detected for each attack name belonging to this type
getTotalAttacksByName <- function(testing.raw.scored.ds, attackType) {
  
  # Get for type who many attackas names are differente
  return (
    testing.raw.scored.ds %>% filter(attack==TRUE & type == attackType) %>% select(attackid,name) %>% group_by(attackid, name) %>% summarise() %>% group_by(name) %>% summarise(n=n())
  )
  
}


# This dataset should have "is attack" as "attack", attack name as 'name' and attack type as 'type'
getDistinctAttacksByType <- function(testing.raw.scored.ds) {

    # Get for type who many attackas names are differente
  return (
      testing.raw.scored.ds %>% filter(attack==TRUE) %>% select(name,type) %>% group_by(name,type) %>% summarise() %>% group_by(type) %>% summarise(n=n())
  )
    
}

# For each type of attack, get how many attacks have been detected
getAttackTypeDetection <- function(testing.raw.scored.ds.log.reduced, th = 0) {
 
  thValue <- th
  
  # Get total attacks per type, that is threshold = 0, and attacks == TRUE
  totalAttacksByType <- getTotalAttacksByType(testing.raw.scored.ds.log.reduced)
  
  # Get attacks  per type, filtered by threshold and attacks == TRUE
  attacksByType <- getTotalAttacksByType(testing.raw.scored.ds.log.reduced %>% 
                              filter(attack==TRUE) %>% filter( scoringNorm > thValue))

  # Do a join between both datasets
  attacksByTypeDetection <- totalAttacksByType %>% left_join(attacksByType, by=c('type')) %>% 
    mutate(percentatge=(n.y/n.x)*100) %>% 
    arrange(type)
  
  # Replaces NA values with 0s
  attacksByTypeDetection[,c("n.y", "percentatge")] <-
    apply(attacksByTypeDetection[,c("n.y","percentatge")], 2, function(x){replace(x, is.na(x), 0)})
  
  
  return(attacksByTypeDetection)
}


# For a type of attack, get detailed detection for each attack name
getAttackTypeDetectionDetailed <- function(testing.raw.scored.ds.log.reduced, attackType, th = 0) {
  
  thValue <- th
  
  # Get total attacks per type, that is threshold = 0, and attacks == TRUE
  totalAttacksByName <- getTotalAttacksByName(testing.raw.scored.ds.log.reduced, attackType)
  
  # Get attacks  per type, filtered by threshold and attacks == TRUE
  attacksByName <- getTotalAttacksByName(testing.raw.scored.ds.log.reduced %>% 
                                           filter(attack==TRUE) %>% filter( scoringNorm > thValue), attackType)
  
  # Do a join between both datasets
  attacksByNameDetection <- totalAttacksByName %>% left_join(attacksByName, by=c('name')) %>% 
    mutate(percentatge=(n.y/n.x)*100) %>% 
    arrange(name)
  
  # Replaces NA values with 0s
  attacksByNameDetection[,c("n.y", "percentatge")] <-
    apply(attacksByNameDetection[,c("n.y","percentatge")], 2, function(x){replace(x, is.na(x), 0)})
  
  
  return(attacksByNameDetection)
}



getAttackTypeCoverage <- function(testing.raw.scored.ds.log.reduced, th = 0) {
  
  thValue <- th
  
  # Recover the number of all distincs attacks possible grouped by type
  distinctAttacksByType <- getDistinctAttacksByType(testing.raw.scored.ds.log.reduced)
  
  # Filter data by threshold (we also filter by attack == TRUE, that is only a memory optimisation, doesn't affect to the result)
  distinctAttacksByTypeTh <- getDistinctAttacksByType(testing.raw.scored.ds.log.reduced %>% 
                            filter(attack==TRUE) %>% filter( scoringNorm > thValue))
  
  # Do a join between both datasets
  coverage <- distinctAttacksByType %>% left_join(distinctAttacksByTypeTh, by=c('type')) %>% 
            mutate(coverage=(n.y/n.x)*100) %>% 
            arrange(type)
  
  # Replaces NA values with 0s
  coverage[,c("n.y", "coverage")] <-
    apply(coverage[,c("n.y","coverage")], 2, function(x){replace(x, is.na(x), 0)})
  
  return(coverage)
  
}



#4.5.2.1.2
renderAttackTypeDetection <- function(scored.ds, threshold = 0, render = FALSE, saveToFile = TRUE ) {

  thValue <- threshold
  typeDetection.ds <- getAttackTypeDetection(scored.ds, thValue ) %>% select(type,percentatge)
  
  if (!render) {
    return (typeDetection.ds)
  } else {
    
    title.label <- paste('% de detección por categoría (th>', threshold,')', sep='')
    x.label  <- 'Categoría'
    y.label  <- '% Detección'
    filename <- paste(getRenderFolder(), '/','coverage_attacktype_',threshold,'.png',sep='')
    
    if( ! saveToFile ) {
      filename <- NULL
    }
    
    render.bar(typeDetection.ds, 
               title.label, 
               x.label,
               y.label, 
              aes(y=typeDetection.ds$percentatge,
                  x=typeDetection.ds$type, 
                  label=paste(round(typeDetection.ds$percentatge,digits=2),"%",sep="")),
              filename = filename)

  }
  
}

#4.5.2.2 - Grpah
renderAttackTypeDetectionDetailed <- function(scored.ds, attackType, threshold = 0, render = FALSE, saveToFile = TRUE ) {
  
  thValue <- threshold
  typeDetection.ds <- getAttackTypeDetectionDetailed(scored.ds, attackType, thValue ) %>% select(name,percentatge)
  
  if (!render) {
    return (typeDetection.ds)
  } else {
    
    title.label <- paste('Ataques detectados de tipo ',attackType, ' (th>', threshold,')', sep='')
    x.label  <- 'Ataques'
    y.label  <- 'Ataques encontrados \n(porcentaje)'
    filename <- paste(getRenderFolder(), '/','coverage_attacktype_detail_',attackType,'_',threshold,'.png',sep='')
    
    if( ! saveToFile ) {
      filename <- NULL
    }
    
    render.bar(typeDetection.ds, 
              title.label, 
              x.label,
              y.label, 
              aes(y=typeDetection.ds$percentatge,
                  x=typeDetection.ds$name, 
                  label=paste(round(typeDetection.ds$percentatge,digits=2),"%",sep="")),
              rotation = 45, filename = filename)
    
  }
  
}


#4.5.2.2 - Table
renderAttackTypeDetectionDetailedTable <- function(scored.ds, attackType, threshold = 0, render = FALSE, saveToFile = FALSE ) {

  thValue <- threshold
  typeDetection.ds <- getAttackTypeDetectionDetailed(scored.ds, attackType, thValue ) %>% select(name,percentatge) %>% arrange(desc(percentatge)) %>% filter(percentatge>0)
  
  if (!render) {
    return (typeDetection.ds)
  } else {
    tableColumnNames <- c('Ataque','Acierto')
    names(typeDetection.ds) <- tableColumnNames
    
    
    typeDetection.ds <- typeDetection.ds %>% 
      mutate(Acierto = paste(format(round(as.double(Acierto),2), nsmall = 2, big.mark=".", decimal.mark = ","),'%',sep=""))
    
    title.label <- paste('TOP detecciones\n(th = ',threshold,')', sep='')
    filename <- paste(getRenderFolder(), '/','top_',attackType,'_',threshold,'.png',sep='')
    
    if( ! saveToFile ) {
      filename <- NULL
    }
    
    render.table(typeDetection.ds, title.label = title.label, filename = filename)
    
  }
  
}


#4.5.2.1.1
renderCoverageType <- function(scored.ds, threshold = 0, render = FALSE, savetoFile = TRUE ) {
  
  thValue <- threshold
  coverage.ds <- getAttackTypeCoverage(scored.ds, thValue ) %>% select(type,coverage)
  
  if (!render) {
    return (coverage.ds)
  } else {
    
    title.label <- paste('Número de ataques distintos \ndetectados por categoría (th>', threshold,')', sep='')
    x.label  <- 'Categoría'
    y.label  <- '% Detección'
    filename <- paste(getRenderFolder(), '/','coverage_',threshold,'.png',sep='')
    
    if( ! savetoFile ) {
      filename <- NULL
    }
    
    render.bar(coverage.ds, 
              title.label, 
              x.label,
              y.label, 
              aes(y=coverage.ds$coverage,
                  x=coverage.ds$type, 
                  label=paste(round(coverage.ds$coverage,digits=2),"%",sep="")), filename = filename)

  }
  
}

renderFPFNTable <- function(scored.ds, threshold = 0, render = TRUE, saveToFile = TRUE ) {
  
  scores.packages.ds <- getFPFNTable(scored.ds, threshold)
  scoresFmt <- format.row(scores.packages.ds,row_number = 1,start_column = 2,decimals = 0)
  scoresFmt <- format.row(scoresFmt,row_number = 2,start_column = 2,decimals = 3,'%')
  
  if (!render) {
    return (scoresFmt)
  } else {
    
    filename = paste(getRenderFolder(), '/','fpfntable_',threshold,'.png',sep='')
    
    if( ! saveToFile ) {
      filename <- NULL
    }
    
    render.table(scoresFmt, title.label = paste('Detecciones por paquetes con Threshold = ',threshold),
                 filename = filename
                 )
  }
  
}

renderFPFNTableComparison  <- function(scored.ds, threshold.1 = 0, threshold.2 = 0, saveToFile = TRUE ) {

  scoresFmt.1 <- renderFPFNTable(scored.ds,threshold.1,FALSE)
  scoresFmt.2 <- renderFPFNTable(scored.ds,threshold.2,FALSE)
  
  title.label.1 <- paste('Detecciones por paquetes con Threshold = ',threshold.1)
  title.label.2 <- paste('Detecciones por paquetes con Threshold = ',threshold.2)
  filename <- paste(getRenderFolder(), '/','fpfntable_',threshold.1,'_',threshold.2,'_compare_', '.png',sep='')
  
  if( ! saveToFile ) {
    filename <- NULL
  }
  
  render.two.tables(scoresFmt.1, title.label.1, scoresFmt.2, title.label.2, filename )
}

renderFPFNByAttackTable <- function(scored.ds, threshold = 0, render = TRUE, saveToFile = TRUE) {
  
  scores.attack.ds <- getFPFNByAttackTable(scored.ds, threshold) 
  
  scoresFmt <- format.row(scores.attack.ds,row_number = 1,start_column = 2,decimals = 0)
  scoresFmt <- format.row(scoresFmt,row_number = 2,start_column = 2,decimals = 3,'%')
  
  if (!render) {
    return (scoresFmt)
  } else {
    
    filename =  paste(getRenderFolder(), '/','fpfn_attack_',threshold,'.png',sep='')
    
    if( ! saveToFile ) {
      filename <- NULL
    }
    
    render.table(scoresFmt, title.label = paste('Detecciones por ataques con Threshold = ',threshold),
                 filename = filename
    )
  }
  
}

renderFPFNByAttackTableComparison <- function(scored.ds, threshold.1 = 0, threshold.2 = 0,  saveToFile = TRUE) {
  
  scoresFmt.1 <- renderFPFNByAttackTable(scored.ds,threshold.1,FALSE)
  scoresFmt.2 <- renderFPFNByAttackTable(scored.ds,threshold.2,FALSE)
  
  title.label.1 <- paste('Detecciones por ataques con Threshold = ',threshold.1)
  title.label.2 <- paste('Detecciones por ataques con Threshold = ',threshold.2)
  
  filename <- paste(getRenderFolder(), '/','fpfn_attack_',threshold.1,'_',threshold.2,'_compare_', '.png',sep='')
  
  
  if( ! saveToFile ) {
    filename <- NULL
  }
  
  
  render.two.tables(scoresFmt.1, title.label.1, scoresFmt.2, title.label.2, filename )
  
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