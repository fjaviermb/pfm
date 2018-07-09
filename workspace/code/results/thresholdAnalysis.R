########################################################################################################
# Threshold analysis
########################################################################################################

########################################################################################################
## 1) Data preparation

# Do normalisation

# Values below 0 move to value 1 to use log10n normalisation and not having problems.
testing.raw.scored.ds.log <-  testing.raw.scored.ds %>% mutate(scoringMin = ifelse(scoring < 1, 1, scoring))

# Apply log10 transformation
testing.raw.scored.ds.log <-  testing.raw.scored.ds.log %>% mutate(scoringLog = log10(scoringMin))

# Rescale between 1 and 100 to apply threshold
testing.raw.scored.ds.log <-  testing.raw.scored.ds.log %>% mutate ( scoringNormLog = lapply( testing.raw.scored.ds.log['scoringLog'], minMaxScaler )[[1]])

# Get only required fields
testing.raw.scored.ds.log.reduced <- testing.raw.scored.ds.log %>% select(scoringLog, scoringNormLog,attack,attackid) %>% mutate ( scoring = scoringLog, scoringNorm = scoringNormLog)

########################################################################################################
# 2) Compare packages beyond Th > 1: red: regular packages, blue: labeled attacks

# Representación de todos los paquetes
dbAttacks <- testing.raw.scored.ds.log.reduced
ggplot() + geom_histogram(data=dbAll, aes(dbAll$scoringNorm), color='blue', fill='blue')+labs(x='Threshold', y='Número paquetes', title='Histograma: todos los paquetes' ) + theme(plot.title = element_text( hjust=0.5, size = rel(1.5), colour = "black"))

# Comparison: packages beyond Th > 1: red: regular packages, blue: labeled attacks
dbAttacks <- testing.raw.scored.ds.log.reduced %>% filter(scoringNorm > 1 & attack == TRUE)
dbAll <- testing.raw.scored.ds.log.reduced %>% filter(scoringNorm > 1)

g1 <- ggplot() + geom_histogram(data=dbAll, aes(dbAll$scoringNorm), color='blue', fill='blue') + geom_histogram(data=dbAttacks, aes(dbAttacks$scoringNorm), color='red', fill='red', bins=100) +labs(x='Threshold', y='Número paquetes', title='Histograma: paquetes normales y ataques (Th>1)' ) + theme(plot.title = element_text( hjust=0.5, size = rel(1.5), colour = "black"))

# Detailed attack packages beyond Th > 1:
dbAttacks <- testing.raw.scored.ds.log.reduced %>% filter(scoringNorm > 1 & attack == TRUE)
g2 <- ggplot() + geom_histogram(data=dbAttacks, aes(dbAttacks$scoringNorm), color='red', fill='red', bins=100) +labs(x='Threshold', y='Número paquetes con ataques', title='Histograma: paquetes ataques (Th>1)' ) + theme(plot.title = element_text( hjust=0.5, size = rel(1.5), colour = "black"))




multiplot(g1,g2,cols=2)

#####

scores <- getFPFNTable(testing.raw.scored.ds.log.reduced, 0)
scoresFmt <- format.row(scores,row_number = 1,start_column = 2,decimals = 0)
scoresFmt <- format.row(scoresFmt,row_number = 2,start_column = 2,decimals = 2,'%')
t1 <- tablerender(scoresFmt)

# Todos los ataques detectados, FN todos
print(renderFPFNTable(testing.raw.scored.ds.log.reduced, 0,FALSE))
# Todos los ataques no detectados, FN Ninguno
print(renderFPFNTable(testing.raw.scored.ds.log.reduced, 100,FALSE))
# Mejores ataques detectados, manteniendo bajo los FN
print(renderFPFNTable(testing.raw.scored.ds.log.reduced, 20,FALSE))
# Mejores ataques detectados, manteniendo un poco mas alto los FN
print(renderFPFNTable(testing.raw.scored.ds.log.reduced, 10,FALSE))

# Render
renderFPFNTable(testing.raw.scored.ds.log.reduced, 22,FALSE)


# Todos los ataques detectados, FN todos
print(renderFPFNByAttackTable(testing.raw.scored.ds.log.reduced, 0,FALSE))
# Todos los ataques no detectados, FN Ninguno
print(renderFPFNByAttackTable(testing.raw.scored.ds.log.reduced, 100,FALSE))
# Mejores ataques detectados, manteniendo bajo los FN
print(renderFPFNByAttackTable(testing.raw.scored.ds.log.reduced, 20,FALSE))
# Mejores ataques detectados, manteniendo un poco mas alto los FN
print(renderFPFNByAttackTable(testing.raw.scored.ds.log.reduced, 10,FALSE))

# Renders utilizados en la documentacion
renderFPFNByAttackTable(testing.raw.scored.ds.log.reduced,1)
renderFPFNTable(testing.raw.scored.ds.log.reduced,1)
renderFPFNTableComparison(testing.raw.scored.ds.log.reduced,1,20)
renderFPFNByAttackTableComparison(testing.raw.scored.ds.log.reduced,1,20)

