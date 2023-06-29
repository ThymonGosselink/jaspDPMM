univariateGaussianDPMM <- function(jaspResults, options, dataset) {
  
  DPMMContainer <- .getDPMMContainer(jaspResults, options)
  
  .createPlotPriorDensity(options, DPMMContainer)
  
  ready <- (length(options$dependent) == 1)
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
  }
  
  .DPMMCheckErrors(options, dataset)
  .DPMMModel(DPMMContainer, options, ready, dataset)
  
  .createTracePlots(DPMMContainer, options, ready)
  
  .createPriorPosteriorPlot(DPMMContainer, options, ready)
  
  .createClusterDensityPlot(DPMMContainer, options, ready, dataset)
  
  .createClusterTable(DPMMContainer, options, ready, dataset)
  
  .clusterPredictionsToData(dataset, options, DPMMContainer, ready) 
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
    dataset <- .scaleDependent(dataset, options)
    dataset <- as.numeric(dataset)
    return(dataset)
  }
}


.scaleDependent <- function(dataset, options) {
  # scale data
  if (options$dependentScaled) {
    dataset <- scale(dataset)
  } else { # added this line of code because if the scale function is not used the data might not be a matrix
    dataset <- as.matrix(dataset)
  }
  return(dataset)
}

.DPMMCheckErrors <- function(options, dataset){
  jaspBase::.hasErrors(dataset, type = c('observations', 'variance', 'infinity'),
             all.target = options$dependent, observations.amount = c('< 3'), 
             exitAnalysisIfErrors = TRUE)
}

.getDPMMContainer <- function(jaspResults,options) {
  if (is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title = "Univariate Dirichlet Process Mixture Model",position = 1)
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    DPMMContainer$dependOn(c("dependent", "scaleDependent", "muPrior", 
                             "kPrior", "alphaPrior", "betaPrior",
                             "mcmcBurnin", "mcmcSamples","alpha",
                             "kluster")
    )
    jaspResults[["DPMMContainer"]] <- DPMMContainer
  }
  return(DPMMContainer)
}


.createPlotPriorDensity <- function(options, DPMMContainer) {
  # plot prior
  if (!options$priorPlot || !is.null(DPMMContainer[["priorPlot"]])) {
    return()
  }
  
  # Create Jasp plot
  priorPlot <- createJaspPlot(title = "Inverse Gamma Prior Plot",  width = 400, height = 500, position = 1)
  #dependencies
  priorPlot$dependOn(options = c("priorPlot"))
  # todo: update citation naar 2023
  priorPlot$addCitation("JASP Team (2018). JASP (Version  17.2.0) [Computer software].")
  # now we assign the plot to jaspResults
  DPMMContainer[["priorPlot"]] <- priorPlot
  
  x <- seq(from=0.1, to=20, by=0.1)
  inverseGamma <- data.frame(x = x, Prior = MCMCpack::dinvgamma(x,
                                           shape = options$alphaPrior,
                                           scale = options$betaPrior))
  # plot distribution
  p <- ggplot2::ggplot(inverseGamma, ggplot2::aes(x = x, y = Prior)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    ggplot2::geom_area(fill = "red", alpha = 0.5) +
    ggplot2::ggtitle("Inverse Gamma Prior") +
    ggplot2::ylab("Density")
  
  priorPlot$plotObject <- p + 
    jaspGraphs::themeJaspRaw()
  return()
}

.DPMMModel <- function(DPMMContainer, options, ready, dataset) {
  if (ready && is.null(DPMMContainer[["model"]])) {
    DPMMContainer[["model"]]$object
    # Take results from state
    # Create the model as a prior mixture model
    if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }
    dp<- dirichletprocess::DirichletProcessGaussian(dataset,
                                                    g0Priors = c(options$muPrior,options$kPrior,options$alphaPrior,options$betaPrior),
                                                    alphaPriors = c(options$alpha,options$kluster))
    
    # Fit the model
    dpFit <- dirichletprocess::Fit(dp, its = options$mcmcSamples, progressBar = TRUE)
    dpBurn <- dirichletprocess::Burn(dpFit, niter = options$mcmcBurnin)
    model <- dpBurn
    DPMMContainer[["model"]] <- createJaspState(object = model)
  }
  return()
}

.createTracePlots <- function(DPMMContainer, options, ready){
  # Trace plots
  if (!ready || !options$tracePlots || !is.null(DPMMContainer[["tracePlotsContrainer"]])) {
    return()
  }
  tracePlotsContrainer <- createJaspContainer(title = "Trace Plots",position = 3)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  tracePlotsContrainer$dependOn(c("traceplots"))
  # save to DPMM container
  DPMMContainer[["tracePlotsContrainer"]] <- tracePlotsContrainer
  
  myModel <- DPMMContainer[["model"]]$object
  
    # Create Jasp plot for Alpha
    tracePlotAlpha <- createJaspPlot(title = "Trace Plot Alpha",  width = 400, height = 500)
    #dependencies
    tracePlotAlpha$dependOn(options = "traceplots")
    tracePlotAlpha$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotAlpha"]] <- tracePlotAlpha
    # Run fill the plot
    tracePlotAlpha$plotObject  <- dirichletprocess::AlphaTraceplot(myModel, gg = TRUE) +
      jaspGraphs::themeJaspRaw()
    
    # Create Jasp plot for Cluster
    tracePlotCluster <- createJaspPlot(title = "Trace Plot Cluster",  width = 400, height = 500)
    #dependencies
    tracePlotCluster$dependOn(options = "tracePlots")
    tracePlotCluster$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotCluster"]] <- tracePlotCluster
    # Run fill the plot
    tracePlotCluster$plotObject  <- dirichletprocess::ClusterTraceplot(myModel, gg = TRUE) +
      jaspGraphs::themeJaspRaw()
    
    # Create Jasp plot for likelihood
    tracePlotLikelihood <- createJaspPlot(title = "Trace Plot Likelihood",  width = 400, height = 500)
    #dependencies
    tracePlotLikelihood$dependOn(options = "tracePlots")
    tracePlotLikelihood$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotLikelihood"]] <- tracePlotLikelihood
    # Run fill the plot
    tracePlotLikelihood$plotObject  <- dirichletprocess::LikelihoodTraceplot(myModel, gg = TRUE) +
                                        jaspGraphs::themeJaspRaw()
  return()  
}

.createPriorPosteriorPlot <- function(DPMMContainer, options, ready){
  # Trace plots
  if (!ready || !options$priorPosteriorPlot || !is.null(DPMMContainer[["priorPosteriorPlotContainer"]])) {
    return()
  }
  priorPosteriorPlotContainer <- createJaspContainer(title = "Prior, Posterior Plot",position = 4)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  priorPosteriorPlotContainer$dependOn(c("priorPosteriorPlot"))
  # save to DPMM container
  DPMMContainer[["PriorPosteriorPlotContainer"]] <- priorPosteriorPlotContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  priorPosteriorPlot <- createJaspPlot(title = "Plot:",  width = 400, height = 500)
  #dependencies
  priorPosteriorPlot$dependOn(options = "priorPosteriorPlot")
  priorPosteriorPlot$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
  # now we assign the plot to jaspResults
  priorPosteriorPlotContainer[["priorPosteriorPlot"]] <- priorPosteriorPlot
  # Run fill the plot
  priorPosteriorPlot$plotObject  <- dirichletprocess::AlphaPriorPosteriorPlot(myModel, gg = TRUE) +
                                    jaspGraphs::themeJaspRaw()
  
  return()  
}

.createClusterDensityPlot <- function(DPMMContainer, options, ready, dataset) {
  # Trace plots
  if (!ready || !options$clusterDensityPlot || !is.null(DPMMContainer[["clusterDensityPlotContainer"]])) {
    return()
  }
  clusterDensityPlotContainer <- createJaspContainer(title = "Cluster Density Plot", position = 5)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  clusterDensityPlotContainer$dependOn(c("clusterDensityPlot"))
  # save to DPMM container
  DPMMContainer[["clusterDensityPlotContainer"]] <- clusterDensityPlotContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  clusterDensityPlot <- createJaspPlot(title = "Plot: ", width = 400, height = 500)
  # dependencies
  clusterDensityPlot$dependOn(options = "clusterdensityplot")
  clusterDensityPlot$addCitation("JASP Team (2023). JASP (Version 17.2.0) [Computer software].")
  # now we assign the plot to jaspResults
  clusterDensityPlotContainer[["clusterDensityPlot"]] <- clusterDensityPlot
  # Run fill the plot
  
  newData <- data.frame(dataset, Clusters = myModel$clusterLabels)
  p <- ggplot2::ggplot(newData, ggplot2::aes(x = dataset, fill = as.factor(Clusters))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::scale_fill_discrete(name = "Cluster") +
    ggplot2::theme_minimal() +
    ggplot2::xlab(colnames(dataset))
  clusterDensityPlot$plotObject <- p +
                                  jaspGraphs::themeJaspRaw()
  return()  
}

.createClusterTable <- function(DPMMContainer, options, ready, dataset){
  if (!ready || !options$tableCluster || !is.null(DPMMContainer[["clusterTableContainer"]])) {
    return()
  }
  
  clusterTableContainer <- createJaspContainer(title = "Cluster Table", position = 2)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  clusterTableContainer$dependOn(c("tableCluster", "clusterAdditionalInfo", 
                                   "clusterCiLevel"))
  # save to DPMM container
  DPMMContainer[["clusterTableContainer"]] <- clusterTableContainer
  
  DPMMClusterTable <- createJaspTable(title = "Table:", position = 2,
                                      dependencies = c("tableCluster", "clusterAdditionalInfo", 
                                                       "clusterCiLevel"))
  DPMMClusterTable$addCitation("JASP Team (2023). JASP (Version 17.2.0) [Computer software].")
  
  
  DPMMClusterTable$addColumnInfo(title = gettext("Clusters"), name = "cluster",   type = "string", combine = TRUE)
  DPMMClusterTable$addColumnInfo(title = gettext("N"), name = "n",  type = "integer")
  DPMMClusterTable$addColumnInfo(title = gettext("Mean"), name = "mean",  type = "number")
  DPMMClusterTable$addColumnInfo(title = gettext("SD"), name = "sd",  type = "number")
  
  
  if (options$clusterAdditionalInfo == TRUE) {
    
  overTitle <- paste0(100 * as.numeric(options[["clusterCiLevel"]]), "% HDI for Proportion")
  DPMMClusterTable$addColumnInfo(title = gettext("Lower HDI"), name = "lowerCi", 
                                 type = "number", 
                                 overtitle = overTitle)
  
  DPMMClusterTable$addColumnInfo(title = gettext("Upper HDI"),name = "upperCi", 
                                 type = "number", 
                                 overtitle = overTitle)
  }
  DPMMClusterTable$showSpecifiedColumnsOnly <- TRUE
  
  DPMMClusterTable$title <- paste0("Table: ", options$dependent, " and Cluster summary")
  
  
  #load model
  myModel <- DPMMContainer[["model"]]$object
  #add data with clusters
  newData <- data.frame(Value = dataset, cluster = myModel$clusterLabels)
 
  
 # calculate HDI per cluster, if and for loop needed because clusters can be different
  if (options$clusterAdditionalInfo) {
    ciHdi <- data.frame(cluster = NA, lowerCi = NA, upperCi = NA)  # Initialize an empty data frame
    
    for (i in 1:myModel$numberClusters) {
      clus <- i
      subsetData <- newData[newData$cluster == clus, ]
      ciResult <- bayestestR::hdi(subsetData$Value, ci = options$clusterCiLevel)
      ciHdi[i, "lowerCi"] <- ciResult$CI_low
      ciHdi[i, "upperCi"] <- ciResult$CI_high
      ciHdi[i,"cluster"] <- clus
    }
    if(any(is.na(ciHdi))){
    DPMMClusterTable$addFootnote(gettext("Some HDI's could not be computed, because N is insufficiently small or the interval % too large. Possibly change the prior, hyperparmeter values or increase iterations to get better cluster estimates or decrease the HDI %."))
    }
    }
  #summarized data
  clusterSummary <- dplyr::group_by(newData, cluster)
  clusterSummary <- dplyr::summarize(clusterSummary,
                                     mean = round(mean(Value),3),
                                     sd = round(sd(Value),3),
                                     n = round(length(Value),0))
  clusterSummary <- merge(clusterSummary, ciHdi, by = "cluster")
                    
  clusterSummary <- as.data.frame(clusterSummary)
  DPMMClusterTable$setExpectedSize(rows = length(clusterSummary$cluster))
 
  
  clusterTableContainer[["DPMMClusterTable"]] <- DPMMClusterTable
  
  DPMMClusterTable$addRows(list(clusters = clusterSummary$clusters, mean = clusterSummary$mean, 
                                sd = clusterSummary$sd, n = clusterSummary$n, 
                                lowerCi = clusterSummary$lowerCi, upperCi = clusterSummary$upperCi))
  DPMMClusterTable$setData(clusterSummary)
  return()
}
  
.clusterPredictionsToData <- function(dataset, options, DPMMContainer, ready) {
  if (!ready || !options[["addPredictions"]] || options[["predictionsColumn"]] == "") {
    return()
  }
  #load model
  myModel <- DPMMContainer[["model"]]$object
  #add data with clusters
  data <- data.frame(Value = dataset, cluster = myModel$clusterLabels)
  if (is.null(DPMMContainer[["predictionsColumn"]])) {
    predictions <- as.character(data[["cluster"]])
    predictionsColumn <- predictions
    predictionsColumn <- factor(predictionsColumn)
    DPMMContainer[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    DPMMContainer[["predictionsColumn"]]$dependOn(options = c("predictionsColumn", "addPredictions"))
    # make sure to create to classification column with the same type as the target!
    DPMMContainer[["predictionsColumn"]]$setOrdinal(predictionsColumn)
  }
  return()
}
