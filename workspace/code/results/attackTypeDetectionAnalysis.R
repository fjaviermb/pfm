# Required to rebuild attack names: based on current  scored
#label.testing.raw.ds <- labelTestingDS(testing.raw.scored.ds, label.attacklist.raw.ds, FALSE, root.dir)
#saveCacheLabelTestingRaw(root.dir)
#label.testing.raw.ds.2 <- label.testing.raw.ds %>% mutate(attackid=NA)
#label.testing.raw.ds.2 <- enrichAttackIdLabelTestingDS(label.testing.raw.ds.2, label.attacklist.raw.ds)
# Poner aqui el scored
# getLabeledTesgingDatan(testing.raw.ds, label.testing.raw.ds, cache = TRUE, root.dir) {
# Replacing cols
# Incase we need to remove olds
#testing.raw.scored.ds <- testing.raw.scored.ds %>% select (-c(attackid1,name1,attack1,attackid, name,attack))
#testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['attack'])
#testing.raw.ds <- bind_cols(testing.raw.ds, label.testing.raw.ds['name'])
# testing.raw.scored.ds[,'name'] <- label.testing.raw.ds[,'name']
# Save new scoring
# saveCacheTestingDatasetScored(root.dir)
# From Here do 1) Data preparation
#######################################################################################################
## 1) Data preparation

# Do normalisation

# Values below 0 move to value 1 to use log10n normalisation and not having problems.
testing.raw.scored.ds.log <-  testing.raw.scored.ds %>% mutate(scoringMin = ifelse(scoring < 1, 1, scoring))

# Apply log10 transformation
testing.raw.scored.ds.log <-  testing.raw.scored.ds.log %>% mutate(scoringLog = log10(scoringMin))

# Rescale between 1 and 100 to apply threshold
testing.raw.scored.ds.log <-  testing.raw.scored.ds.log %>% mutate ( scoringNormLog = lapply( testing.raw.scored.ds.log['scoringLog'], minMaxScaler )[[1]])

# Get only required fields
testing.raw.scored.ds.log.reduced <- testing.raw.scored.ds.log %>% select(scoringLog, scoringNormLog,attack,attackid, name) %>% mutate ( scoring = scoringLog, scoringNorm = scoringNormLog)

# Load attacktypes
label.attacktypes.raw.ds <- loadLabelAttackTypes(cache = TRUE, root.dir) 

# De donde sacamos el reduced??
# Get only required fields
testing.raw.scored.ds.log.reduced <- testing.raw.scored.ds.log %>% select(scoringLog, scoringNormLog,attack,attackid, name) %>% mutate ( scoring = scoringLog, scoringNorm = scoringNormLog)

# Add category or type attack to current dataset
testing.raw.scored.ds.log.reduced <- testing.raw.scored.ds.log.reduced %>% left_join(
  label.attacktypes.raw.ds, by=c('name') )

########################################################################################################
# 2) Compare attacks beyond Th > 1

# 2.1) 
#thValue <- 1
#getAttackTypeCoverage(testing.raw.scored.ds.log.reduced,1 )
#ggplot(data=d, aes(y=d$coverage,x=d$type)) + geom_bar(stat="identity", fill = '#4A90D2', color = '#4F93D0') +labs(x='Categoría', y='% Detección', title='Número de ataques distintos \ndetectados por categoría')+  theme(plot.title = element_text( hjust=0.5, size = rel(1.5), colour = "black")) + geom_text(aes(label=paste(round(d$coverage,digits=2),"%",sep="")), vjust=-0.3) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = backgroundColor, colour = NA))

renderCoverageType(testing.raw.scored.ds.log.reduced, th = 1, TRUE)

#2.2) Porcentajes por tipo de ataque
renderAttackTypeDetection(testing.raw.scored.ds.log.reduced, th = 1, TRUE)

#2.3) Render por cada tipo de ataque
renderAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'DOS', th = 1, TRUE)
renderAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'DAT', th = 1, TRUE)
renderAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'URA', th = 1, TRUE)
renderAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'RLA', th = 1, TRUE)
renderAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'PRO', th = 1, TRUE)
renderAttackTypeDetectionDetailedTable(testing.raw.scored.ds.log.reduced, attackType = 'DOS', th = 1, TRUE)
renderAttackTypeDetectionDetailedTable(testing.raw.scored.ds.log.reduced, attackType = 'DAT', th = 1, TRUE)
renderAttackTypeDetectionDetailedTable(testing.raw.scored.ds.log.reduced, attackType = 'URA', th = 1, TRUE)
renderAttackTypeDetectionDetailedTable(testing.raw.scored.ds.log.reduced, attackType = 'RLA', th = 1, TRUE)
renderAttackTypeDetectionDetailedTable(testing.raw.scored.ds.log.reduced, attackType = 'PRO', th = 1, TRUE)

#getAttackTypeDetectionDetailed(testing.raw.scored.ds.log.reduced, attackType = 'DOS', th = 20)