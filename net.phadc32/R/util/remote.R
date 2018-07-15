
getCacheTrainingRawFromURL <- function() {
  urlItem <- ('https://www.dropbox.com/s/wju6rht88bxjjkg/training.raw.ds.cache.RData.gz?raw=1')
  conn = gzcon(url(urlItem))
  load(conn)
  close(conn)
  return(training.raw.ds.cache)
}

getCacheTestingRawFromURL <- function() {
  urlItem <- ('https://www.dropbox.com/s/vh79uqt2iyhfq0r/testing.raw.ds.cache.RData.gz?raw=1')
  conn = gzcon(url(urlItem))
  load(conn)
  close(conn)
  return(testing.raw.ds.cache)
}

getCacheLabelTestingRawFromURL <- function() {
  urlItem <- ('https://www.dropbox.com/s/dyezwt2qm87zpm6/label.testing.raw.ds.cache.RData.gz?raw=1')
  conn = gzcon(url(urlItem))
  load(conn)
  close(conn)
  return(label.testing.raw.ds.cache)
}


