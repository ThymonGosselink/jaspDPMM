DPMM <- function(jaspResults, options, dataset) {
  ready <- (length(options$dependent) == 1)
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
  }
  
  .plotpriordens(options, jaspResults)
  DPMMContainer <- .getDPMMContainer(jaspResults)
  DPMMContainer[["model"]] <- .DPMMModelContainer(DPMMContainer, options, ready, dataset)
  
  return(jaspResults)   
}

.DPMMReadData <- function(options,dataset) {
  # read data
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric = options$dependent)
    dataset <- .scaledependent(dataset, options)
    dataset <- as.numeric(dataset)
    return(dataset)
  }
}


.scaledependent <- function(dataset, options) {
  # scale data
  if (options$scaledependent == TRUE) {
    dataset <- scale(dataset)
  }
  return(dataset)
}


.plotpriordens <- function(options, jaspResults) {
  # plot prior
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
  # fill the plot
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


.getDPMMContainer <- function(jaspResults) {
  if (is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title = "Univariate Dirichlet Process Mixture Model",position = 1)
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    DPMMContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                              "k0", "alpha0", "beta0",
                              "mcmcBurnin", "mcmcSamples","alpha", 
                              "kluster", "traceplots", "priorposteriorplot",
                              "clusterdensityplot", "tablecluster"
                              )
                            )
    jaspResults[["DPMMContainer"]] <- DPMMContainer
  }
  return(jaspResults[["DPMMContainer"]])
}

.DPMMModelContainer <- function(DPMMContainer, options, ready, dataset) {
  if (ready && is.null(DPMMContainer[["model"]])) {
    # Take results from state
    model <- .DPMMModel(dataset, options)
    print(model)
    DPMMContainer[["model"]] <- createJaspState(model)
    
  }
  return(model)
}

.DPMMModel <- function(dataset, options) {
# Create the model as a prior mixture model
dp<- dirichletprocess::DirichletProcessGaussian(dataset,
                              g0Priors = c(options$mu0,options$k0,options$alpha0,options$beta0),
                              alphaPriors = c(options$alpha,options$kluster))

# Fit the model
dpfit <- dirichletprocess::Fit(dp, its = options$mcmcSamples, progressBar = TRUE)
dpburn <- dirichletprocess::Burn(dpfit, niter = options$mcmcBurnin)
model <- dpburn
return(model)
}
  
