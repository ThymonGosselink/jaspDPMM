univariateGaussianDPMM <- function(jaspResults, options, dataset) {

  ready <- (length(options[["dependent"]]) > 0)
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
  }
  
  .DPMMCheckErrors(options, dataset)
  
  DPMMContainer <- .getDPMMContainer(jaspResults, options, ready)
  
  .createPlotPriorDensityAlpha(options, DPMMContainer, ready)
  
 
  .DPMMModel(DPMMContainer, options, ready, dataset)
  
  .createTracePlots(DPMMContainer, options, ready)
  
  .createPriorPosteriorPlot(DPMMContainer, options, ready)
  
  .createClusterDensityPlot(DPMMContainer, options, ready, dataset)
  
  .createClusterTable(DPMMContainer, options, ready, dataset)
  
  .clusterPredictionsToData(dataset, options, DPMMContainer, ready) 
  return()
}



.DPMMReadData <- function(options,dataset) {
  # read data
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric = options[["dependent"]])
    dataset <- .scaleDependent(dataset, options)
    dataset <- as.numeric(dataset)
    return(dataset)
  }
}


.scaleDependent <- function(dataset, options) {
  # scale data
  if (options[["dependentScaled"]]) {
    dataset <- scale(dataset)
  } else { # added this line of code because if the scale function is not 
    # used the data might not be a matrix
    dataset <- as.matrix(dataset)
  }
  return(dataset)
}

.DPMMCheckErrors <- function(options, dataset) {
  jaspBase::.hasErrors(dataset, type = c('observations', 'variance', 'infinity'),
                       all.target = options[["dependent"]], observations.amount = c('< 3'), 
                       exitAnalysisIfErrors = TRUE)
  return()
}

.getDPMMContainer <- function(jaspResults, options, ready) {
  if(!ready){ 
    return()
  }
  
  if(is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title    = "Univariate Dirichlet Process Mixture Model",
                                         position = 1)
    # Set the dependencies on the container
    DPMMContainer$dependOn(c("dependent", "dependentScaled", "muBasePrior", 
                             "kBasePrior", "alphaBasePrior", "betaBasePrior",
                             "mcmcBurnin", "mcmcSamples","aAlphaPrior",
                             "bAlphaPrior")
    )
    jaspResults[["DPMMContainer"]] <- DPMMContainer
  }
  DPMMContainer <- jaspResults[["DPMMContainer"]]
  return(DPMMContainer)
}


.createPlotPriorDensityAlpha <- function(options, DPMMContainer, ready) {
  # plot prior
  if (!options[["priorPlot"]] || !is.null(DPMMContainer[["priorPlot"]]) || !ready) {
    return()
  }
  
  # Create Jasp plot
  priorPlot <- createJaspPlot(title = "Gamma Distribution Plot of Alpha Prior",  width = 500, height = 600, position = 1)
  
  priorPlot$dependOn(options = c("priorPlot"))
  
  priorPlot$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
  
  DPMMContainer[["priorPlot"]] <- priorPlot
  
  x <- seq(from=0.1, to=40, by=0.1)
  
  # create the distribution
  gamma <- data.frame(x = x, Prior = dgamma(x,
                                            shape = options[["aAlphaPrior"]],
                                            scale = options[["bAlphaPrior"]]))
  # plot distribution
  p <- ggplot2::ggplot(gamma, ggplot2::aes(x = x, y = Prior)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    ggplot2::geom_area(fill = "red", alpha = 0.5) +
    ggplot2::ggtitle("Gamma Prior") +
    ggplot2::ylab("Density")
  
  priorPlot$plotObject <- p + 
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return()
}

.DPMMModel <- function(DPMMContainer, options, ready, dataset) {
  if (!ready || !is.null(DPMMContainer[["model"]])) {
    return()
  }
    
  DPMMContainer[["model"]]$object
    # Take results from state
    # Create the model as a prior mixture model
  if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }
  
  # set the options
  dp<- dirichletprocess::DirichletProcessGaussian(dataset,
                                                  g0Priors = c(options[["muBasePrior"]],
                                                               options[["kBasePrior"]],
                                                               options[["alphaBasePrior"]],
                                                               options[["betaBasePrior"]]),
                                                  alphaPriors = c(options[["aAlphaPrior"]],
                                                                  options[["bAlphaPrior"]]))
  
  # Fit the model
  dpFit <- dirichletprocess::Fit(dp, 
                                 its = options[["mcmcSamples"]], 
                                 progressBar = TRUE)
  # specify burnin
  dpBurn <- dirichletprocess::Burn(dpFit, 
                                   niter = options[["mcmcBurnin"]])
  
  # save it
  model <- dpBurn
  DPMMContainer[["model"]] <- createJaspState(object = model)
  
  return()
}

.createTracePlots <- function(DPMMContainer, options, ready) {
  
  # Trace plots
  if (!ready || !options[["tracePlots"]] || !is.null(DPMMContainer[["tracePlotsContainer"]])) {
    return()
  }
  
  tracePlotsContainer <- createJaspContainer(title = "Trace Plots",position = 3)
  # Set the dependencies on the container.
  tracePlotsContainer$dependOn(c("tracePlots"))
  # save to DPMM container
  DPMMContainer[["tracePlotsContainer"]] <- tracePlotsContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  
  
  # Create Jasp plot for Alpha
  tracePlotAlpha <- createJaspPlot(title = "Trace Plot Alpha",  width = 500, height = 600)
  tracePlotAlpha$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].")
  tracePlotsContainer[["tracePlotAlpha"]] <- tracePlotAlpha
  # Run fill the plot
  tracePlotAlpha$plotObject  <- dirichletprocess::AlphaTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() + 
    jaspGraphs::themeJaspRaw()
  
 
  
   # Create Jasp plot for Cluster
  tracePlotCluster <- createJaspPlot(title = "Trace Plot Cluster",  width = 500, height = 600)
  tracePlotCluster$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].")
  # now we assign the plot to jaspResults
  tracePlotsContainer[["tracePlotCluster"]] <- tracePlotCluster
  # Run fill the plot
  tracePlotCluster$plotObject  <- dirichletprocess::ClusterTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  
  
  # Create Jasp plot for likelihood
  tracePlotLikelihood <- createJaspPlot(title = "Trace Plot Likelihood",  width = 500, height = 600)
  # tracePlotLikelihood$dependOn(options = "tracePlots")
  tracePlotLikelihood$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].")
  # now we assign the plot to jaspResults
  tracePlotsContainer[["tracePlotLikelihood"]] <- tracePlotLikelihood
  # Run fill the plot
  tracePlotLikelihood$plotObject  <- dirichletprocess::LikelihoodTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  
  return()  
}

.createPriorPosteriorPlot <- function(DPMMContainer, options, ready){
  # Trace plots
  if(!ready || !options[["priorPosteriorPlot"]] || !is.null(DPMMContainer[["priorPosteriorPlotContainer"]])) {
    return()
  }
  priorPosteriorPlotContainer <- createJaspContainer(title = "Prior, Posterior Plot",position = 4)
  priorPosteriorPlotContainer$dependOn(c("priorPosteriorPlot"))
  # save to DPMM container
  DPMMContainer[["priorPosteriorPlotContainer"]] <- priorPosteriorPlotContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  priorPosteriorPlot <- createJaspPlot(title = "Plot:",  width = 500, height = 600)
  #dependencies
  priorPosteriorPlot$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].")
  # now we assign the plot to jaspResults
  priorPosteriorPlotContainer[["priorPosteriorPlot"]] <- priorPosteriorPlot
  # Run fill the plot
  priorPosteriorPlot$plotObject  <- dirichletprocess::AlphaPriorPosteriorPlot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return()  
}

.createClusterDensityPlot <- function(DPMMContainer, options, ready, dataset) {
  # Trace plots
  if(!ready || !options[["clusterDensityPlot"]] || !is.null(DPMMContainer[["clusterDensityPlotContainer"]])) {
    return()
  }
  clusterDensityPlotContainer <- createJaspContainer(title = "Cluster Density Plot", position = 5)
  clusterDensityPlotContainer$dependOn(c("clusterDensityPlot"))
  # save to DPMM container
  DPMMContainer[["clusterDensityPlotContainer"]] <- clusterDensityPlotContainer
  
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  clusterDensityPlot <- createJaspPlot(title = "Plot: ", width = 500, height = 600)
  clusterDensityPlot$addCitation("JASP Team (2023). JASP (Version 17.2.1) [Computer software].")
  # now we assign the plot to jaspResults
  clusterDensityPlotContainer[["clusterDensityPlot"]] <- clusterDensityPlot
  
  # Run fill the plot
  clusterData <- data.frame(dataset, Cluster = myModel[["clusterLabels"]])
  
  p <- ggplot2::ggplot(clusterData, ggplot2::aes(x = dataset, fill = as.factor(Cluster))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::scale_fill_discrete(name = "Cluster") +
    ggplot2::theme_minimal() +
    ggplot2::xlab(colnames(dataset))
  
  clusterDensityPlot$plotObject <- p +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return()  
}

.createClusterTable <- function(DPMMContainer, options, ready, dataset) {
  if (!ready || !options[["tableCluster"]] || !is.null(DPMMContainer[["clusterTableContainer"]])) {
    return()
  }
  
  clusterTableContainer <- createJaspContainer(title = "Cluster Table", position = 2)
  clusterTableContainer$dependOn(c("tableCluster", "clusterAdditionalInfo", 
                                   "clusterCiLevel"))
  # save to DPMM container
  DPMMContainer[["clusterTableContainer"]] <- clusterTableContainer
  
  DPMMClusterTable <- createJaspTable(title = "Table:", position = 2,
                                      dependencies = c("tableCluster", "clusterAdditionalInfo", 
                                                       "clusterCiLevel"))
  DPMMClusterTable$addCitation("JASP Team (2023). JASP (Version 17.2.0) [Computer software].")
  
  # Cluster column
  DPMMClusterTable$addColumnInfo(title = gettext("Cluster"), name    = "cluster",   
                                 type  = "string",            combine = TRUE)
  # N column
  DPMMClusterTable$addColumnInfo(title = gettext("N"), name = "n",  type = "integer")
  
  # Mean column
  DPMMClusterTable$addColumnInfo(title = gettext("Mean"), name = "mean",  type = "number")
  
  # SD column
  DPMMClusterTable$addColumnInfo(title = gettext("SD"), name = "sd",  type = "number")
  
  # add Highest density interval column
  if (options[["clusterAdditionalInfo"]]) {
    
    overTitle <- paste0(100 * as.numeric(options[["clusterCiLevel"]]), "% HDI for Proportion")
    DPMMClusterTable$addColumnInfo(title = gettext("Lower HDI"), name = "lowerCi", 
                                   type = "number", 
                                   overtitle = overTitle)
    
    DPMMClusterTable$addColumnInfo(title = gettext("Upper HDI"),name = "upperCi", 
                                   type = "number", 
                                   overtitle = overTitle)
  }
  DPMMClusterTable$showSpecifiedColumnsOnly <- TRUE
  
  DPMMClusterTable$title <- paste0("Table: ", options[["dependent"]], " and Cluster summary")
  
  
  #load model
  myModel <- DPMMContainer[["model"]]$object
  #add data with clusters
  clusterData <- data.frame(Value = dataset, cluster = myModel[["clusterLabels"]])
  
  # calculate HDI per cluster, if and for loop needed because clusters can be different
  if (options[["clusterAdditionalInfo"]]) {
    ciHdi <- data.frame(cluster = NA, lowerCi = NA, upperCi = NA)  # Initialize an empty data frame
    
    # get the values
    for (i in 1:myModel[["numberClusters"]]) {
      clus <- i
      subsetData <- clusterData[clusterData[["cluster"]] == clus, ]
      ciResult <- bayestestR::hdi(subsetData$Value, ci = options[["clusterCiLevel"]])
      ciHdi[i, "lowerCi"] <- ciResult[["CI_low"]]
      ciHdi[i, "upperCi"] <- ciResult[["CI_high"]]
      ciHdi[i,"cluster"] <- clus
    }
    if(any(is.na(ciHdi))) { # If any NA's because N is probably to small
      DPMMClusterTable$addFootnote(
        gettext(paste0("Some HDI's could not be computed, ", 
                       "because N is insufficiently small or the interval % too large, decrease the HDI % . ", 
                       "Possibly change the prior values, hyperparmeter values or ", 
                       "increase iterations, to get better cluster estimates.")))
    }
  }
  #summarized data
  clusterSummary <- dplyr::group_by(clusterData, cluster)
  clusterSummary <- dplyr::summarize(clusterSummary,
                                     mean = round(mean(Value),3),
                                     sd = round(sd(Value),3),
                                     n = round(length(Value),0))
  # add the HDI
  if(options[["clusterAdditionalInfo"]]){
  clusterSummary <- merge(clusterSummary, ciHdi, by = "cluster")
  
  clusterSummary <- as.data.frame(clusterSummary)
  }
  DPMMClusterTable$setExpectedSize(rows = length(clusterSummary[["cluster"]]))
  
  
  clusterTableContainer[["DPMMClusterTable"]] <- DPMMClusterTable
  
  DPMMClusterTable$addRows(list(cluster = clusterSummary[["cluster"]], mean = clusterSummary[["mean"]], 
                                sd = clusterSummary[["sd"]], n = clusterSummary[["n"]], 
                                lowerCi = clusterSummary[["lowerCi"]], upperCi = clusterSummary[["upperCi"]]))
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
  clusterData <- data.frame(Value = dataset, cluster = myModel[["clusterLabels"]])
  
  if (is.null(DPMMContainer[["predictionsColumn"]])) {
    predictions <- as.character(clusterData[["cluster"]])
    predictionsColumn <- predictions
    predictionsColumn <- factor(predictionsColumn)
    DPMMContainer[["predictionsColumn"]] <- createJaspColumn(columnName = options[["predictionsColumn"]])
    DPMMContainer[["predictionsColumn"]]$dependOn(options = c("predictionsColumn", "addPredictions"))
    
    DPMMContainer[["predictionsColumn"]]$setOrdinal(predictionsColumn)
  }
  return()
}
