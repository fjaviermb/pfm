# Clusterer version of python one from ml-ids project (https://github.com/lukehsiao/ml-ids/blob/master/utils/clusterer.py)
library(R6)

# Required: install.packages("hashmap")
# Soporte hashmap
# https://cran.r-project.org/web/packages/hashmap/README.html

# 'Data structure for clustering continuous data.

# 'Data structure for use in PHAD-C. Stores a list of ranges or clusters up to
# 'a maximum of C (32 by default). If C is exceeded during training, then we
# 'find the two closest ranges and merge them.
# 
# ' Usage
# '   clusterTest <- Clusterer$new()
# '   clusterTest <- Clusterer$new(C=4)
# '   clusterTest$add(2)
# '
Clusterer <- R6Class("Clusterer",
                     
  public = list (
    
      C = NULL,
      R = 0,
      N = 0,
      clusters = NULL,

      # ' Initialize the Clusterer.
      initialize = function(C = 32) {
        self$C <- C   # Maximum number of clusters
        self$R <- 0   # Approximation of distinct values added
        self$N <- 0   # Total number of values added
        self$clusters <- data.frame(distance=numeric(), min=numeric(), max=numeric(),stringsAsFactors=FALSE)
      },
      # ' Add a new value to the cluster.
      add = function(value) {
 
        # Increment total values counter
        self$N <- self$N + 1
        
        if( self$N > 1 ) {
          
          if ( ! ( self$contains(value)) ) {
            
            # Add item to list and inc distinct value counter
            self$clusters <-rbind( self$clusters, c(0,value,value) )
            names(self$clusters) <- c("distance","min","max")
            
            # Sort list of nodes
            self$clusters <-self$clusters[(order(self$clusters$min)),]
            
            self$R  <- self$R + 1 
            
            # Merge clusters if necessary to maintain maximum C
            if( nrow(self$clusters) > self$C ) {
              
              # Compute distances between each range (ignoring first wraparound)
              for(rangePos in 2:nrow(self$clusters)){
                # distancia entre min de 1 y max del 1+1
                distance <- self$clusters[rangePos,2] - self$clusters[rangePos-1,3]   
                self$clusters[rangePos-1,1] <- distance
              }
              
              # Add a big distance on last element
              self$clusters[nrow(self$clusters),1] <- 99999999
              
              # Search min index
              minIndex <- which.min(self$clusters$distance)
              
              # Do merge and remove old one
              #self$clusters[minIndex][1] = self.clusters[minIndex + 1][1]
              #self$clusters.remove(self.clusters[minIndex + 1])
              
              # Change the max value, we keep min value
              self$clusters[minIndex,3] <- self$clusters[minIndex+1,3]
              
              # Remove old one
              self$clusters <- self$clusters[-c(minIndex+1),]
              
            }
            
          }
          
          
        } else {
          
          # Add item to list and inc distinct value counter
          self$clusters <-rbind( self$clusters, c(0,value,value) )
          names(self$clusters) <- c("distance","min","max")
          
          # Sort list of nodes
          self$clusters <-self$clusters[(order(self$clusters$min)),]
          
          self$R  <- self$R + 1 
          
        }
  
      },
      #' Return the approximation of distinct values seen.
      getDistinct = function() {
        getDistinct <- self$R
      },
      #' Return the total number of values added.
      getTotal = function() {
        getTotal <- self$N
      },
      #' Return the list of ranges.
      getClusters = function() {
        getClusters <- self$clusters
      },
      # ' Clear the contents of the Clusterer.      
      clear = function() {
        self$R <- 0
        self$N <- 0
        self$clusters <- data.frame(distance=numeric(), min=numeric(), max=numeric(),stringsAsFactors=FALSE)
      },
      # ' Check if the value falls into any existing cluster.
      contains = function(value) {

        for(rangePos in 1:nrow(self$clusters)){
          
          min <- self$clusters[rangePos,2]  
          max <- self$clusters[rangePos,3]
          
          
          if( value >= min && value <= max) {
            return(TRUE)
          }
        }
        
        return(FALSE)
        
      }
   )
)

