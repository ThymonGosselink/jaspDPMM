DPMM <- function(jaspResults, options, dataset) {
  
  ready <- (length(options$dependent) == 1)
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
  }
  
  DPMMContainer <- .getDPMMContainer(jaspResults)
  
  .createPlotPriorDens(options, DPMMContainer)
  
  .DPMMModel(DPMMContainer, options, ready, dataset)
  
  .createTracePlots(DPMMContainer, options, ready)
  
  .createPriorPosteriorPlot(DPMMContainer, options, ready)
  
  .createClusterDensityPlot(DPMMContainer, options, ready, dataset)
  
  .createClusterTable(DPMMContainer, options, ready, dataset)
  
  return()   
}

.useData <- function(DPMMContainer, options){
  

  myModel <- DPMMContainer[["model"]]$object
  
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

.getDPMMContainer <- function(jaspResults) {
  if (is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title = "Univariate Dirichlet Process Mixture Model",position = 1)
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    DPMMContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                             "k0", "alpha0", "beta0",
                             "mcmcBurnin", "mcmcSamples","alpha", 
                             "kluster", "traceplots", "priorposteriorplot",
                             "clusterdensityplot", "tablecluster")
    )
    jaspResults[["DPMMContainer"]] <- DPMMContainer
  }
  return(DPMMContainer)
}


.createPlotPriorDens <- function(options, DPMMContainer) {
  # plot prior
  if (options$plotprior == FALSE | !is.null(DPMMContainer[["plotPrior"]])) {
    return()
  }
  
  # Create Jasp plot
  plotPrior <- createJaspPlot(title = "Inverse Gamma Prior Plot",  width = 400, height = 500)
  #dependencies
  plotPrior$dependOn(options = c("plotPrior", "alpha0", "beta0"))
  # todo: update citation naar 2023
  plotPrior$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  # now we assign the plot to jaspResults
  DPMMContainer[["plotPrior"]] <- plotPrior
  
  x <- seq(from=0.1, to=20, by=0.1)
  invgamma <- data.frame(x = x, Prior = MCMCpack::dinvgamma(x,
                                           shape = options$alpha0,
                                           scale = options$beta0))
  # plot distribution
  p <- ggplot2::ggplot(invgamma, ggplot2::aes(x = x, y = Prior)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    ggplot2::geom_area(fill = "red", alpha = 0.5) +
    ggplot2::ggtitle("Inverse Gamma Prior")
  
  plotPrior$plotObject <- p  # Save the plot under "priorplot"
  
  return()
}

.DPMMModel <- function(DPMMContainer, options, ready, dataset) {
  if (ready && is.null(DPMMContainer[["model"]])) {
    DPMMContainer[["model"]]$object
    # Take results from state
    # Create the model as a prior mixture model
    set.seed(123)
    dp<- dirichletprocess::DirichletProcessGaussian(dataset,
                                                    g0Priors = c(options$mu0,options$k0,options$alpha0,options$beta0),
                                                    alphaPriors = c(options$alpha,options$kluster))
    
    # Fit the model
    dpfit <- dirichletprocess::Fit(dp, its = options$mcmcSamples, progressBar = TRUE)
    dpburn <- dirichletprocess::Burn(dpfit, niter = options$mcmcBurnin)
    model <- dpburn
    DPMMContainer[["model"]] <- createJaspState(object = model)
  }
  return()
}

.createTracePlots <- function(DPMMContainer, options, ready){
  # Trace plots
  if (options$plotprior == FALSE | !is.null(DPMMContainer[["tracePlotsContrainer"]])) {
    return()
  }
  tracePlotsContrainer <- createJaspContainer(title = "Trace Plots",position = 2)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  tracePlotsContrainer$dependOn(c("dependent", "scaledependent", "mu0", 
                                  "k0", "alpha0", "beta0",
                                  "mcmcBurnin", "mcmcSamples","alpha", 
                                  "kluster", "traceplots"))
  # save to DPMM container
  DPMMContainer[["tracePlotsContrainer"]] <- tracePlotsContrainer
  
  myModel <- DPMMContainer[["model"]]$object
  
    # Create Jasp plot for Alpha
    tracePlotsAlpha <- createJaspPlot(title = "Trace Plot Alpha",  width = 400, height = 500)
    #dependencies
    tracePlotsAlpha$dependOn(options = "traceplots")
    tracePlotsAlpha$addCitation("JASP Team (2023). JASP (Version 0.9.2) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsAlpha"]] <- tracePlotsAlpha
    # Run fill the plot
    tracePlotsAlpha$plotObject  <- dirichletprocess::AlphaTraceplot(myModel, gg = TRUE)
    
    # Create Jasp plot for Cluster
    tracePlotsCluster <- createJaspPlot(title = "Trace Plot Cluster",  width = 400, height = 500)
    #dependencies
    tracePlotsCluster$dependOn(options = "traceplots")
    tracePlotsCluster$addCitation("JASP Team (2023). JASP (Version 0.9.2) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsCluster"]] <- tracePlotsCluster
    # Run fill the plot
    tracePlotsCluster$plotObject  <- dirichletprocess::ClusterTraceplot(myModel, gg = TRUE)
    
    # Create Jasp plot for likelihood
    tracePlotsLikelihood <- createJaspPlot(title = "Trace Plot Likelihood",  width = 400, height = 500)
    #dependencies
    tracePlotsLikelihood$dependOn(options = "traceplots")
    tracePlotsLikelihood$addCitation("JASP Team (2023). JASP (Version 0.9.2) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsLikelihood"]] <- tracePlotsLikelihood
    # Run fill the plot
    tracePlotsLikelihood$plotObject  <- dirichletprocess::LikelihoodTraceplot(myModel, gg = TRUE)
    
  return()  
}

.createPriorPosteriorPlot <- function(DPMMContainer, options, ready){
  # Trace plots
  if (options$priorposteriorplot == FALSE | !is.null(DPMMContainer[["PriorPosteriorPlotContainer"]])) {
    return()
  }
  PriorPosteriorPlotContainer <- createJaspContainer(title = "Prior, Posterior Plot",position = 2)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  PriorPosteriorPlotContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                                  "k0", "alpha0", "beta0",
                                  "mcmcBurnin", "mcmcSamples","alpha", 
                                  "kluster", "priorposteriorplot"))
  # save to DPMM container
  DPMMContainer[["PriorPosteriorPlotContainer"]] <- PriorPosteriorPlotContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  PriorPosteriorPlot <- createJaspPlot(title = "Plot:",  width = 400, height = 500)
  #dependencies
  PriorPosteriorPlot$dependOn(options = "priorposteriorplot")
  PriorPosteriorPlot$addCitation("JASP Team (2023). JASP (Version 0.9.2) [Computer software].")
  # now we assign the plot to jaspResults
  PriorPosteriorPlotContainer[["PriorPosteriorPlot"]] <- PriorPosteriorPlot
  # Run fill the plot
  PriorPosteriorPlot$plotObject  <- dirichletprocess::AlphaPriorPosteriorPlot(myModel, gg = TRUE)
  
  return()  
}

.createClusterDensityPlot <- function(DPMMContainer, options, ready, dataset) {
  # Trace plots
  if (options$clusterdensityplot == FALSE | !is.null(DPMMContainer[["clusterDensityPlotContainer"]])) {
    return()
  }
  clusterDensityPlotContainer <- createJaspContainer(title = "Cluster Density Plot", position = 3)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  clusterDensityPlotContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                                         "k0", "alpha0", "beta0",
                                         "mcmcBurnin", "mcmcSamples","alpha", 
                                         "kluster", "clusterdensityplot"))
  # save to DPMM container
  DPMMContainer[["clusterDensityPlotContainer"]] <- clusterDensityPlotContainer
  
  mymodel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  clusterDensityPlot <- createJaspPlot(title = "Plot:", width = 400, height = 500)
  # dependencies
  clusterDensityPlot$dependOn(options = "clusterdensityplot")
  clusterDensityPlot$addCitation("JASP Team (2023). JASP (Version 0.9.2) [Computer software].")
  # now we assign the plot to jaspResults
  clusterDensityPlotContainer[["clusterDensityPlot"]] <- clusterDensityPlot
  # Run fill the plot
  
  newData <- data.frame(dataset, Clusters = mymodel$clusterLabels)
  clusterDensityPlot$plotObject <- ggplot2::ggplot(newData, ggplot2::aes(x = dataset, fill = as.factor(Clusters))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::scale_fill_discrete(name = "Cluster") +
    ggplot2::theme_minimal() +
    ggplot2::xlab(colnames(dataset))
  
  return()  
}

.createClusterTable <- function(DPMMContainer, options, ready, dataset){
  if (options$tablecluster == FALSE | !is.null(DPMMContainer[["clusterTableContainer"]])) {
    return()
  }
  
  clusterTableContainer <- createJaspContainer(title = "Cluster Table", position = 4)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  clusterTableContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                                         "k0", "alpha0", "beta0",
                                         "mcmcBurnin", "mcmcSamples","alpha", 
                                         "kluster", "tablecluster"))
  # save to DPMM container
  DPMMContainer[["clusterTableContainer"]] <- clusterTableContainer
  
  DPMMClusterTable <- createJaspTable(title = "Table:", position = 5,
                                dependencies = c("dependent", "scaledependent", "mu0", 
                                                 "k0", "alpha0", "beta0",
                                                 "mcmcBurnin", "mcmcSamples","alpha", 
                                                 "kluster", "tablecluster"))
  DPMMClusterTable$addCitation("JASP Team (2023). JASP (Version 17.1.0) [Computer software].")
  
  
  DPMMClusterTable$addColumnInfo(title = "Clusters", name = "clusters",   type = "string", combine = TRUE)
  DPMMClusterTable$addColumnInfo(title = gettext("Mean"), name = "mean",  type = "number")
  DPMMClusterTable$addColumnInfo(title = gettext("SD"), name = "sd",  type = "number")
  
  DPMMClusterTable$showSpecifiedColumnsOnly <- TRUE
  
  DPMMClusterTable$title <- paste0("Table: ", options$dependent)
  
  #load model
  mymodel <- DPMMContainer[["model"]]$object
  #add data with clusters
  newData <- data.frame(Value = dataset, clusters = mymodel$clusterLabels)
  
  #summarized data
  clusterSummary <- dplyr::group_by(newData, clusters)
  clusterSummary <- dplyr::summarize(clusterSummary,
                                      mean = round(mean(Value),3),
                                      sd = round(sd(Value),3))
  clusterSummary <- as.data.frame(clusterSummary)
  print(clusterSummary)
  DPMMClusterTable$setExpectedSize(rows = length(clusterSummary$cluster))
  
  
  clusterTableContainer[["DPMMClusterTable"]] <- DPMMClusterTable
  
  
  

  
  
  DPMMClusterTable$addRows(list(clusters = clusterSummary$clusters, mean = clusterSummary, sd = clusterSummary$sd))
  DPMMClusterTable$setData(clusterSummary)
  return()
}
  

