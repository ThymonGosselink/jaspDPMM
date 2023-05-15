


DPMM <- function(jaspResults, dataset, options) {
  ready <- (length(options$variables) = 1)
  
  if (ready)
    dataset <- DPMMReadData(dataset, options)
 
}


DPMMReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    return(.readDataSetToEnd(columns.as.factor = options$variables))
}







