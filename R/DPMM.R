DPMM <- function(jaspResults, options, dataset) {
  
  DPMMContainer <- .getDPMMContainer(jaspResults, options)
  
  .createPlotPriorDens(options, DPMMContainer)
  
  ready <- (length(options$dependent) == 1)
  if (ready) {
    dataset <- .DPMMReadData(options, dataset)
    .DPMMCheckErrors(options, dataset)
  } else {
    .DPMMCheckErrors(options, dataset)
  }
  
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
  } else { # added this line of code because if the scale function is not used the data might not be a matrix
    dataset <- as.matrix(dataset)
  }
  return(dataset)
}

.DPMMCheckErrors <- function(options, dataset){
  customCheck <- list(function(dataset, options) {
    if(options$dependent == 0){
      return(gettext("Add a variable!"))
    }
    return()
  }
  )
  jaspBase::.hasErrors(dataset, type = c('observations', 'variance', 'infinity'), custom = customCheck,
             all.target = options$dependent, observations.amount = c('< 3'), 
             exitAnalysisIfErrors = TRUE)
}

.getDPMMContainer <- function(jaspResults,options) {
  if (is.null(jaspResults[["DPMMContainer"]])) {
    #create container
    DPMMContainer <- createJaspContainer(title = "Univariate Dirichlet Process Mixture Model",position = 1)
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    DPMMContainer$dependOn(c("dependent", "scaledependent", "mu0", 
                             "k0", "alpha0", "beta0",
                             "mcmcBurnin", "mcmcSamples","alpha",
                             "kluster")
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
  plotPrior <- createJaspPlot(title = "Inverse Gamma Prior Plot",  width = 400, height = 500, position = 1)
  #dependencies
  plotPrior$dependOn(options = c("plotPrior", "alpha0", "beta0"))
  # todo: update citation naar 2023
  plotPrior$addCitation("JASP Team (2018). JASP (Version  17.2.0) [Computer software].")
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
    ggplot2::ggtitle("Inverse Gamma Prior") +
    ggplot2::ylab("Density")
  
  plotPrior$plotObject <- p + 
    jaspGraphs::themeJaspRaw()
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
  if (options$traceplots == FALSE | !is.null(DPMMContainer[["tracePlotsContrainer"]])) {
    return()
  }
  tracePlotsContrainer <- createJaspContainer(title = "Trace Plots",position = 3)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  tracePlotsContrainer$dependOn(c("dependent", "scaledependent", "mu0", 
                                  "k0", "alpha0", "beta0",
                                  "mcmcBurnin", "mcmcSamples","alpha", 
                                  "kluster"))
  # save to DPMM container
  DPMMContainer[["tracePlotsContrainer"]] <- tracePlotsContrainer
  
  myModel <- DPMMContainer[["model"]]$object
  
    # Create Jasp plot for Alpha
    tracePlotsAlpha <- createJaspPlot(title = "Trace Plot Alpha",  width = 400, height = 500)
    #dependencies
    tracePlotsAlpha$dependOn(options = "traceplots")
    tracePlotsAlpha$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsAlpha"]] <- tracePlotsAlpha
    # Run fill the plot
    tracePlotsAlpha$plotObject  <- dirichletprocess::AlphaTraceplot(myModel, gg = TRUE) +
      jaspGraphs::themeJaspRaw()
    
    # Create Jasp plot for Cluster
    tracePlotsCluster <- createJaspPlot(title = "Trace Plot Cluster",  width = 400, height = 500)
    #dependencies
    tracePlotsCluster$dependOn(options = "traceplots")
    tracePlotsCluster$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsCluster"]] <- tracePlotsCluster
    # Run fill the plot
    tracePlotsCluster$plotObject  <- dirichletprocess::ClusterTraceplot(myModel, gg = TRUE) +
      jaspGraphs::themeJaspRaw()
    
    # Create Jasp plot for likelihood
    tracePlotsLikelihood <- createJaspPlot(title = "Trace Plot Likelihood",  width = 400, height = 500)
    #dependencies
    tracePlotsLikelihood$dependOn(options = "traceplots")
    tracePlotsLikelihood$addCitation("JASP Team (2023). JASP (Version  17.2.0) [Computer software].")
    # now we assign the plot to jaspResults
    tracePlotsContrainer[["tracePlotsLikelihood"]] <- tracePlotsLikelihood
    # Run fill the plot
    tracePlotsLikelihood$plotObject  <- dirichletprocess::LikelihoodTraceplot(myModel, gg = TRUE) +
                                        jaspGraphs::themeJaspRaw()
  return()  
}

.createPriorPosteriorPlot <- function(DPMMContainer, options, ready){
  # Trace plots
  if (options$priorPosteriorPlot == FALSE | !is.null(DPMMContainer[["priorPosteriorPlotContainer"]])) {
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
  if (options$clusterDensityPlot == FALSE | !is.null(DPMMContainer[["clusterDensityPlotContainer"]])) {
    return()
  }
  clusterDensityPlotContainer <- createJaspContainer(title = "Cluster Density Plot", position = 5)
  # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
  clusterDensityPlotContainer$dependOn(c("clusterDensityPlot"))
  # save to DPMM container
  DPMMContainer[["clusterDensityPlotContainer"]] <- clusterDensityPlotContainer
  
  mymodel <- DPMMContainer[["model"]]$object
  
  # Create Jasp plot for Prior posterior plot
  clusterDensityPlot <- createJaspPlot(title = "Plot: ", width = 400, height = 500)
  # dependencies
  clusterDensityPlot$dependOn(options = "clusterdensityplot")
  clusterDensityPlot$addCitation("JASP Team (2023). JASP (Version 17.2.0) [Computer software].")
  # now we assign the plot to jaspResults
  clusterDensityPlotContainer[["clusterDensityPlot"]] <- clusterDensityPlot
  # Run fill the plot
  
  newData <- data.frame(dataset, Clusters = mymodel$clusterLabels)
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
  if (options$tableCluster == FALSE | !is.null(DPMMContainer[["clusterTableContainer"]])) {
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
  DPMMClusterTable$addFootnote(gettextf("If the sample size (N) is insufficiently small, it is not be possible to calculate the HDI and and will be epmty"))
  
  #load model
  mymodel <- DPMMContainer[["model"]]$object
  #add data with clusters
  newData <- data.frame(Value = dataset, cluster = mymodel$clusterLabels)
 
  
 
  if (options$clusterAdditionalInfo == TRUE) {
    ci_hdi <- data.frame(cluster = NA, lowerCi = NA, upperCi = NA)  # Initialize an empty data frame
    
    for (i in 1:mymodel$numberClusters) {
      clus <- i
      subset_data <- newData[newData$cluster == clus, ]
      if(dplyr::count(subset_data) > 10) {
      ci_result <- bayestestR::hdi(subset_data$Value, ci = options$clusterCiLevel)
      ci_hdi[i,"cluster"] <- clus
      ci_hdi[i, "lowerCi"] <- ci_result$CI_low
      ci_hdi[i, "upperCi"] <- ci_result$CI_high
      } else {
        ci_hdi[i,"cluster"] <- clus
        ci_hdi[i, "lowerCi"] <- NA
        ci_hdi[i,"upperCi"] <- NA
    }
    
   
    }
  }
  #summarized data
  clusterSummary <- dplyr::group_by(newData, cluster)
  clusterSummary <- dplyr::summarize(clusterSummary,
                                     mean = round(mean(Value),3),
                                     sd = round(sd(Value),3),
                                     n = round(length(Value),0))
  clusterSummary <- merge(clusterSummary, ci_hdi, by = "cluster")
                    
  clusterSummary <- as.data.frame(clusterSummary)
  print(clusterSummary)
  DPMMClusterTable$setExpectedSize(rows = length(clusterSummary$cluster))
 
  
  clusterTableContainer[["DPMMClusterTable"]] <- DPMMClusterTable
  
  DPMMClusterTable$addRows(list(clusters = clusterSummary$clusters, mean = clusterSummary$mean, 
                                sd = clusterSummary$sd, n = clusterSummary$n, 
                                lowerCi = clusterSummary$lowerCi, upperCi = clusterSummary$upperCi))
  DPMMClusterTable$setData(clusterSummary)
  return()
}
  

