DPMM <- function(jaspResults,options, dataset) {
  ready <- (length(options$dependent) == 1)
  
  if(ready) {
    dataset <- .DPMMReadData(options, dataset)
  }
  
  .plotpriordens(options, jaspResults)

  return()   
}

.DPMMReadData <- function(options,dataset) {
  
  if (!is.null(dataset)) {
    return(dataset)
  } else {
 dataset <- return(.readDataSetToEnd(columns.as.numeric  = options$dependent))
 .scaledependent(dataset,options)
  }
  return()
}

.scaledependent <- function(dataset, options) {
  if(options$scaledependent == TRUE) {
    dataset <- stats::scale(dataset)
    return(dataset)
  } else {
    return(dataset)
  }
}

.plotpriordens <- function(options, jaspResults) {
  if (options$plotprior == TRUE && is.null(jaspResults[["plotprior"]])) {
      jaspResults[["plotprior"]]$object
      # Create Jasp plot
      plotprior <- createJaspPlot(title = "Plot Prior",  width = 400, height = 500)
      #dependencies
      plotprior$dependOn(options = c("plotprior", "alpha0", "beta0"))
      plotprior$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
      # now we assign the plot to jaspResults
      jaspResults[["plotprior"]] <- plotprior
      # Run fill the plot
  priorplot  <-  .priorFillPlotDescriptives(plotprior, options, jaspResults)
  
    plotprior$plotObject <- priorplot
  }
  return()
}

.priorFillPlotDescriptives <- function(plotprior, options, jaspResults){
  
  # create inverse gamma distribution
  invgamma <- data.frame(Prior = MCMCpack::rinvgamma(n = 2000, shape = options$alpha0,scale = options$beta0))
  
  # plot distribution
  priorplot <-  ggplot2::ggplot(invgamma, ggplot2::aes(x = invgamma$Prior, y = ..density..)) +
                ggplot2::geom_histogram(colour = "black", fill = "#2c7fb8", bins = 30) +
                ggplot2::geom_density(data = invgamma, ggplot2::aes(x = invgamma$Prior), 
                                      color = "red", fill = "red", alpha = 0.5) +
                ggplot2::ggtitle(label = "Inverse Gamma Prior")
  
  
  
  return(priorplot)
}

