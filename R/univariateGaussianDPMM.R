univariateGaussianDPMM <- function(jaspResults, options, dataset = NULL) {

  ready <- (options[["dependent"]] != "")
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
    .DPMMCheckErrors(options, dataset)
  }
  
  
  
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
           # used the data might not be a matrix. 
           # The dirichlet process would like a matrix and not a list
    dataset <- as.matrix(dataset)
  }
  return(dataset)
}

.DPMMCheckErrors <- function(options, dataset) {
  # check basic data errors
  jaspBase::.hasErrors(dataset, type = c('observations', 'variance', 'infinity'),
                       all.target = options[["dependent"]], observations.amount = c('< 3'), 
                       exitAnalysisIfErrors = TRUE)
  return()
}


# main container
.getDPMMContainer <- function(jaspResults, options, ready) {
  
  
  if(is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title    = "Univariate Dirichlet Process Mixture Model",
                                         position = 1)
    
    # Set the dependencies on the container. These dependencies are responsible for the DPMM to work
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

# Prior plot of alpha
.createPlotPriorDensityAlpha <- function(options, DPMMContainer, ready) {
  # plot prior
  if (!options[["priorPlot"]] || !is.null(DPMMContainer[["priorPlot"]])) {
    return()
  }
  
  # Create Jasp plot
  priorPlot <- createJaspPlot(title = "Gamma Distribution Plot of Alpha Prior",  width = 500, height = 600, position = 1)
  
  priorPlot$dependOn(options = c("priorPlot"))
  
  priorPlot$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].,
                        Ross, G. J., & Markwick, D. (2018). dirichletprocess: 
                        An R package for fitting complex Bayesian nonparametric models.")
  
  DPMMContainer[["priorPlot"]] <- priorPlot
  
  # using qgamma to make sure the range is right
  x <- seq(from = 0, to = qgamma(.999999999, shape = options[["aAlphaPrior"]],
                                 scale = options[["bAlphaPrior"]]), 
                                 by = 0.1)
           
  
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

# get model information
.DPMMModel <- function(DPMMContainer, options, ready, dataset) {
  if (!ready || !is.null(DPMMContainer[["model"]])) {
    return()
  }
    
  DPMMContainer[["model"]]$object
  
  # set seed for reproducebility
  if (options[["setSeed"]]) {
    set.seed(options[["seed"]])
  }
  
  # Create the model as a prior mixture model with all the options
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


# create trace plots for model diagnostics.
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
  
  # Create and fill the three plots:
  # - Create trace plot for Alpha:
  tracePlotAlpha <- createJaspPlot(title = "Trace Plot Alpha",  width = 500, height = 600)
  tracePlotAlpha$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].,
                             Ross, G. J., & Markwick, D. (2018). dirichletprocess:
                             An R package for fitting complex Bayesian nonparametric models.")
  tracePlotsContainer[["tracePlotAlpha"]] <- tracePlotAlpha
  # Fill the plot
  tracePlotAlpha$plotObject  <- dirichletprocess::AlphaTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() + 
    jaspGraphs::themeJaspRaw()
  
 
  
  # - Create trace plot for Cluster:
  tracePlotCluster <- createJaspPlot(title = "Trace Plot Cluster",  width = 500, height = 600)
  tracePlotCluster$addCitation("JASP Team (2023). JASP (Version  17.2.1) [Computer software].
                               Ross, G. J., & Markwick, D. (2018). dirichletprocess:
                               An R package for fitting complex Bayesian nonparametric models.")
  # assign the plot to jaspResults
  tracePlotsContainer[["tracePlotCluster"]] <- tracePlotCluster
  # Run fill the plot
  tracePlotCluster$plotObject  <- dirichletprocess::ClusterTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  
  
  # Create Jasp plot for likelihood:
  tracePlotLikelihood <- createJaspPlot(title = "Trace Plot Likelihood",  width = 500, height = 600)
  # tracePlotLikelihood$dependOn(options = "tracePlots")
  tracePlotLikelihood$addCitation("JASP Team (2023). JASP (Version  17.3.0) [Computer software].
                                  Ross, G. J., & Markwick, D. (2018). 
                                  dirichletprocess: An R package for fitting complex Bayesian nonparametric models.")
  # now we assign the plot to jaspResults
  tracePlotsContainer[["tracePlotLikelihood"]] <- tracePlotLikelihood
  # Run fill the plot
  tracePlotLikelihood$plotObject  <- dirichletprocess::LikelihoodTraceplot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  
  return()  
}


# The prior posterior plot:
.createPriorPosteriorPlot <- function(DPMMContainer, options, ready){
  if(!ready || !options[["priorPosteriorPlot"]] || !is.null(DPMMContainer[["priorPosteriorPlotContainer"]])) {
    return()
  }
  priorPosteriorPlotContainer <- createJaspContainer(title = "Prior, Posterior Plot",position = 4)
  
  priorPosteriorPlotContainer$dependOn(c("priorPosteriorPlot"))
  # save to DPMM container
  DPMMContainer[["priorPosteriorPlotContainer"]] <- priorPosteriorPlotContainer
  
  # get model information
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  priorPosteriorPlot <- createJaspPlot(title = "Plot:",  width = 500, height = 600)
  priorPosteriorPlot$addCitation("JASP Team (2023). JASP (Version  17.3.0) [Computer software],
                                 Ross, G. J., & Markwick, D. (2018).
                                 dirichletprocess: An R package for fitting complex Bayesian nonparametric models.")
  
  # now we assign the plot to jaspResults
  priorPosteriorPlotContainer[["priorPosteriorPlot"]] <- priorPosteriorPlot
  
  # Run fill the plot
  priorPosteriorPlot$plotObject  <- dirichletprocess::AlphaPriorPosteriorPlot(myModel, gg = TRUE) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  
  return()  
}

# display the density for each cluster.
.createClusterDensityPlot <- function(DPMMContainer, options, ready, dataset) {
  if(!ready || !options[["clusterDensityPlot"]] || !is.null(DPMMContainer[["clusterDensityPlotContainer"]])) {
    return()
  }
  clusterDensityPlotContainer <- createJaspContainer(title = "Cluster Density Plot", position = 5)
  clusterDensityPlotContainer$dependOn(c("clusterDensityPlot"))
  # save to DPMM container
  DPMMContainer[["clusterDensityPlotContainer"]] <- clusterDensityPlotContainer
  
  # get model information
  myModel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  clusterDensityPlot <- createJaspPlot(title = "Plot: ", width = 500, height = 600)
  clusterDensityPlot$addCitation("JASP Team (2023). JASP (Version 17.2.1) [Computer software],
                                 Ross, G. J., & Markwick, D. (2018). dirichletprocess: 
                                 An R package for fitting complex Bayesian nonparametric models.")
  # now we assign the plot to jaspResults
  clusterDensityPlotContainer[["clusterDensityPlot"]] <- clusterDensityPlot
  
  # Run fill the plot:
  
  # get the cluster labels
  clusterData <- data.frame(dataset, Cluster = myModel[["clusterLabels"]])
  
  # run the plot
  p <- ggplot2::ggplot(clusterData, ggplot2::aes(x = dataset, fill = as.factor(Cluster))) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::xlab(colnames(dataset)) + 
    ggplot2::scale_x_continuous(limits = c(min(dataset)-1, max(dataset)+1))
  
  clusterDensityPlot$plotObject <- p +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() + 
    ggplot2::theme(legend.position="top") +
    ggplot2::scale_fill_discrete(name = "Cluster")
  
  return()  
}

# A nice table with dependent and cluster information
.createClusterTable <- function(DPMMContainer, options, ready, dataset) {
  if (!options[["tableCluster"]] || !is.null(DPMMContainer[["clusterTableContainer"]])) {
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
  DPMMClusterTable$addCitation("JASP Team (2023). JASP (Version 17.3.0) [Computer software].")
  
  # Cluster column
  DPMMClusterTable$addColumnInfo(title = gettext("Cluster"), name    = "cluster",   
                                 type  = "string",            combine = TRUE)
  # N column
  DPMMClusterTable$addColumnInfo(title = gettext("N"), name = "n",  type = "integer")
  
  # Mean column
  DPMMClusterTable$addColumnInfo(title = gettext("Mean"), name = "mean",  type = "number")
  
  # SD column
  DPMMClusterTable$addColumnInfo(title = gettext("SD"), name = "sd",  type = "number")
  
  # Weight column
  DPMMClusterTable$addColumnInfo(title = gettext("Weight"), name = "weight",  type = "number")
  
  # add Highest density interval column If options additional = TRUE
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
  
  clusterTableContainer[["DPMMClusterTable"]] <- DPMMClusterTable
  
  if(ready){
  #load model
  myModel <- DPMMContainer[["model"]]$object
  #add data with clusters
  clusterData <- data.frame(Value = dataset, cluster = myModel[["clusterLabels"]])
  
  # calculate HDI per cluster, if and for loop needed. ONLY if additional info = TRUE
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
  clusterSummary <- dplyr::group_by(clusterData, cluster)
  
  # summarize
  clusterSummary <- dplyr::summarize(clusterSummary,
                                     mean = round(mean(Value), 3),
                                     sd = round(sd(Value), 3),
                                     n = round(length(Value), 0))
  # add weights
  clusterSummary <- cbind(clusterSummary, weight = myModel[["weights"]])
 
   # add the HDI IF additional info = TRUE
  if(options[["clusterAdditionalInfo"]]){
  clusterSummary <- merge(clusterSummary, ciHdi, by = "cluster")
  
  clusterSummary <- as.data.frame(clusterSummary)
  }
  
  # expected row size = amount of clusters
  DPMMClusterTable$setExpectedSize(rows = length(clusterSummary[["cluster"]]))
  
  
  
  
  # add the rows
  DPMMClusterTable$addRows(list(cluster = clusterSummary[["cluster"]], mean = clusterSummary[["mean"]], 
                                sd = clusterSummary[["sd"]], n = clusterSummary[["n"]], 
                                weight = clusterSummary[["weight"]],
                                lowerCi = clusterSummary[["lowerCi"]], upperCi = clusterSummary[["upperCi"]]))
  
  # add the data
  DPMMClusterTable$setData(clusterSummary)
  
  }
  return()
}


# Add the predictions to the data.
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
