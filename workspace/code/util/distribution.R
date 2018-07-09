
getWorker <- function() {
  
  # Get current worker. By default no worker or invalid worker
  worker.current <- -1
  
  if( ! is.na(Sys.getenv()["worker_all"]) & !  is.na(Sys.getenv()["worker_current"])) {
    worker.all <- as.numeric(Sys.getenv("worker_all"))
    worker.current <- as.numeric(Sys.getenv("worker_current"))  
  }
  
  return (worker.current)
  
}
